{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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
import qualified Data.Aeson as JSON
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Symptom json
    name String
    deriving Show
SymptomPoint
    seriesId SymptomId
    value    Int
    deriving Show
Food
    name String
    description String
    ingredients [FoodId]
    deriving Show
Meal
    pictureURL  String Maybe
    notes       String
    needsReview Bool
    foods       [FoodId]
    deriving Show
|]

data App = App
    { connPool :: ConnectionPool
    }


mkYesod "App" [parseRoutes|
/ HomeR GET
/add-symptom AddSymptomR POST
/symptoms SymptomsR GET
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

postAddDocR :: Handler JSON.Value
postAddDocR = undefined

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
