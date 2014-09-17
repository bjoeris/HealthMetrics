{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}

-- | Main entry point to the application.
module Main where

import           Control.Applicative                     ((<$>), (<*>))
import           Control.Monad                           (forM)
import           Control.Monad.Logger                    (runStdoutLoggingT)
import           Data.Conduit
import qualified Data.Conduit.List                       as CL
import           Data.Maybe                              (catMaybes)
import           Data.Monoid                             (mconcat)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.XML.Types                          as X
import           Database.Persist.Sqlite
import           Text.Blaze.Html                         (preEscapedToHtml)
import           Text.XML.Stream.Render                  (def, renderBuilder)
import           Data.Time.LocalTime
import           Data.Aeson                              (fromJSON)
import qualified Data.HashMap.Strict as HashMap
import           GHC.Generics
import Network.HTTP.Types as Import
    ( status200
    , status201
    , status400
    , status403
    , status404
    )
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Symptom json
    name String
    deriving Show
SymptomPoint json
    symptomId SymptomId
    value     Int
    time      ZonedTime
    deriving Show
Food json
    name        String
    description String
    deriving Show
FoodIngredients json
    ownerId      FoodId
    ingredientId FoodId
    amount       Double
Meal json
    pictureURL  String Maybe
    notes       String
    needsReview Bool
    deriving Show
MealIngredients json
    mealId MealId
    foodId FoodId
    amount Double
|]

data PartialSymptomPoint =
    PartialSymptomPoint Int ZonedTime
    deriving (Show,Generic)
instance FromJSON PartialSymptomPoint

data App = App
    { connPool :: ConnectionPool
    }


mkYesod "App" [parseRoutes|
/                                           HomeR GET
/symptoms                                   SymptomsR GET POST
/symptoms/#SymptomId                        SymptomR GET PUT DELETE
/symptoms/#SymptomId/points                 SymptomPointsR GET POST
/symptoms/#SymptomId/points/#SymptomPointId SymptomPointR GET PUT DELETE
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    symptomCount <- runDB $ count ([] :: [Filter Symptom])
    let symptoms = if symptomCount == 1
                then "There is currently 1 symptom."
                else "There are currently " ++ show symptomCount ++ " symptoms."
    defaultLayout [whamlet|
<p>Welcome to the health metrics application. #{symptoms}
|]

getSymptomsR :: Handler Value
getSymptomsR = do
    symptoms <- runDB $ selectList [] [] :: Handler [Entity Symptom]
    return $ object ["symptoms" .= symptoms]

postSymptomsR :: Handler Value
postSymptomsR = do
    symptom <- parseJsonBody_ :: Handler Symptom
    symptomId <- runDB $ insert symptom
    sendResponseStatus status201 $ object ["symptomId" .= symptomId]

getSymptomR :: SymptomId -> Handler Value
getSymptomR symptomId = do
    symptom <- runDB $ get404 symptomId
    return $ toJSON symptom

putSymptomR :: SymptomId -> Handler Text
putSymptomR symptomId = do
    symptom <- parseJsonBody_ :: Handler Symptom
    _ <- runDB $ repsert symptomId symptom
    return "UPDATED"

deleteSymptomR :: SymptomId -> Handler Text
deleteSymptomR symptomId = do
    _ <- runDB $ delete symptomId
    return "DELETED"

getSymptomPointsR :: SymptomId -> Handler Value
getSymptomPointsR symptomId = do
    symptomPoints <- runDB $ selectList [SymptomPointSymptomId ==. symptomId] [] :: Handler [Entity SymptomPoint]
    return $ toJSON symptomPoints

postSymptomPointsR :: SymptomId -> Handler ()
postSymptomPointsR symptomId = do
    partialSymptomPoint <- parseJsonBody_ :: Handler PartialSymptomPoint
    let symptomPoint = 
            case partialSymptomPoint of
                PartialSymptomPoint value time -> SymptomPoint symptomId value time
    symptomPointId <- runDB $ insert symptomPoint
    sendResponseStatus status201 $ object ["symptomPointId" .= symptomPointId]

getSymptomPointR :: SymptomId -> SymptomPointId -> Handler Value
getSymptomPointR _ symptomPointId = undefined

putSymptomPointR :: SymptomId -> SymptomPointId -> Handler ()
putSymptomPointR _ symptomPointId = undefined

deleteSymptomPointR :: SymptomId -> SymptomPointId -> Handler ()
deleteSymptomPointR _ symptomPointId = undefined

runApp :: Text -> IO ()
runApp dbString = withSqlitePool dbString 10 $ \pool -> do
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    warpEnv $ App pool

main :: IO ()
--main = runApp "health.db3"
main = runApp ":memory:"
