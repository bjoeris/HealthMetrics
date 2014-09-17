{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test where

import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Monadic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.LocalTime
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Data.Sequence (Seq,(|>),(<|),(><),viewl,ViewL(EmptyL,(:<)))
import qualified Data.Sequence as Seq
import Data.Aeson
import Data.Aeson.Types
--import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Text.Read (readMaybe)
import System.Environment (getEnv)

import Main (runApp)

type SymptomId = Int
type SymptomPointId = Int

data ModelSymptomPoint = ModelSymptomPoint
    { symptomPointValue :: Int
    , symptomPointTime :: ZonedTime
    }
    deriving (Show)

data ModelSymptom = ModelSymptom
    { symptomName :: String
    , symptomPoints :: Map SymptomPointId ModelSymptomPoint
    }
    deriving (Show)

data Model = Model
    { modelSymptoms :: Map SymptomId ModelSymptom
    , nextSymptomId :: SymptomId
    , nextSymptomPointId :: SymptomPointId }
    deriving (Show)

data ActionResult
    = EmptyResult Status
    | JSONResult Status Value
    | JSONError Status String
    deriving (Eq,Show)

data Action = Action
    { actionRequest     :: String -> Request
    , actionModelUpdate :: Model -> (ActionResult, Model)
    }
instance Show Action where
    show act = show (actionRequest act "<root>")

performAction :: String -> Action -> IO ActionResult
performAction rootUrl act = do
    let request' = actionRequest act rootUrl
        request = request { checkStatus = \_ _ _ -> Nothing }
    response <- withManager $ httpLbs request
    let status = responseStatus response
    let rawBody = responseBody response
    if LBS.null rawBody then
        return $ EmptyResult status
    else
        case eitherDecode rawBody of
            Left errorString -> return $ JSONError status errorString
            Right value -> return $ JSONResult status value

data Path
    = RootPath
    | SymptomsPath
    | SymptomPath Int
    | SymptomPointsPath Int
    | SymptomPointPath Int Int
    | BadPath Path String


alphaNumChar :: Gen Char
alphaNumChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

alphaNumString :: Gen String
alphaNumString = listOf alphaNumChar

genSymptomId :: Model -> Gen SymptomId
genSymptomId model = if Map.null (modelSymptoms model) then newSymptomId
                     else oneof [ existingSymptomId, newSymptomId ]
    where
        existingSymptomId = elements (Map.keys (modelSymptoms model))
        newSymptomId = suchThat arbitrary
            (\k -> Map.notMember k (modelSymptoms model))

genSymptomPointId :: Model -> SymptomId -> Gen SymptomPointId
genSymptomPointId model symptomId = case Map.lookup symptomId (modelSymptoms model) of
    Nothing -> arbitrary
    (Just symptom) ->
        let existingSymptomPointId = elements (Map.keys (symptomPoints symptom))
            newSymptomPointId = suchThat arbitrary (\k -> Map.notMember k (symptomPoints symptom))
        in if Map.null (modelSymptoms model) then newSymptomPointId
           else oneof [ existingSymptomPointId, newSymptomPointId ]

genPath :: Model -> Gen Path
genPath model = oneof
    [ {-return Root
    ,-} return SymptomsPath
    , symptomPath
    , symptomPointsPath
    , symptomPointPath
    , badPath ]
    where
        symptomPath = liftM SymptomPath (genSymptomId model)
        symptomPointsPath = liftM SymptomPointsPath (genSymptomId model)
        symptomPointPath = do
            symptomId <- genSymptomId model
            symptomPointId <- genSymptomPointId model symptomId
            return $ SymptomPointPath symptomId symptomPointId
        badPath = oneof [ badRootPath, badSymptomsPath, badSymptomPath, badSymptomPointsPath, badSymptomPointPath ]
        badRootPath         = do
            str <- suchThat alphaNumString (/= "symptoms")
            return $ BadPath RootPath str
        badSymptomsPath     = do
            str <- suchThat alphaNumString
                            (\str -> isNothing (readMaybe str :: Maybe SymptomId))
            return $ BadPath SymptomsPath str
        badSymptomPath      = do
            str       <- suchThat alphaNumString (/= "points")
            symptomId <- genSymptomId model
            return $ BadPath (SymptomPath symptomId) str
        badSymptomPointsPath     = do
            str <- suchThat alphaNumString
                            (\str -> isNothing (readMaybe str :: Maybe SymptomId))
            symptomId <- genSymptomId model
            return $ BadPath (SymptomPointsPath symptomId) str
        badSymptomPointPath = do
            str            <- alphaNumString
            symptomId      <- genSymptomId model
            symptomPointId <- genSymptomPointId model symptomId
            return $ BadPath (SymptomPointPath symptomId symptomPointId) str

instance Show Path where
    show RootPath = "/"
    show SymptomsPath = "/symptoms"
    show (SymptomPath symptomId) = "/symptoms/" ++ (show symptomId)
    show (SymptomPointsPath symptomId) = "/symptoms/" ++ (show symptomId) ++ "/points"
    show (SymptomPointPath symptomId symptomPointId) = "/symptoms/" ++ (show symptomId) ++ "/points/" ++ (show symptomPointId)
    show (BadPath prefix suffix) = (show prefix) ++ "/" ++ suffix

genMethod :: Path -> Gen Method
genMethod SymptomsPath = oneof
    [ elements [ methodGet, methodPost ]
    , elements [ methodPut, methodDelete, methodHead ]]
genMethod (SymptomPath symptomId) = oneof
    [ elements [ methodGet, methodPut, methodDelete ]
    , elements [ methodPost, methodHead ]]
genMethod (SymptomPointsPath symptomId) = oneof
    [ elements [ methodGet, methodPost ]
    , elements [ methodPut, methodDelete, methodHead ]]
genMethod (SymptomPointPath symptomId symptomPointId) = oneof
    [ elements [ methodGet, methodPut, methodDelete ]
    , elements [ methodPost, methodHead ]]
genMethod _ = elements
    [ methodGet
    , methodPost
    , methodHead
    , methodPut
    , methodDelete ]

genBody :: Path -> Method -> Gen BS.ByteString
genBody path method = undefined

modelUpdate :: Path -> Method -> Maybe Value -> State Model ActionResult
modelUpdate SymptomsPath method body
    | method == methodGet  = case body of
        Nothing -> do
            model <- get
            return $ JSONResult ok200 (
                object ["symptoms" .= map (\(k,v) -> object [ "key" .= k
                                                            , "value" .= object ["name" .= symptomName v]])
                                          (Map.assocs (modelSymptoms model))])
        _ -> return $ EmptyResult badRequest400
    | method == methodPost = 
        let nameParser :: Value -> Parser String
            nameParser (Object v) = v .: "name"
            nameParser _ = mzero in
                case  body >>= parseMaybe nameParser of
                    Just name -> do
                        model @ Model {..} <- get
                        put $ model { modelSymptoms = Map.insert nextSymptomId (ModelSymptom name Map.empty) modelSymptoms
                                    , nextSymptomId = nextSymptomId + 1 }
                        return $ JSONResult ok200 $ object ["symptomId" .= nextSymptomId]
                    Nothing -> return $ EmptyResult badRequest400
    | otherwise            = return $ EmptyResult badRequest400
modelUpdate (SymptomPath symptomId) method body
    | method == methodGet = undefined
    | method == methodPut = undefined
    | method == methodDelete = undefined
    | otherwise = undefined
modelUpdate (SymptomPointsPath symptomId) method bodp
    | method == methodGet = undefined
    | method == methodPost = undefined
    | otherwise = undefined
modelUpdate (SymptomPointPath symptomId symptomPointId) method body
    | method == methodGet = undefined
    | method == methodPut = undefined
    | method == methodDelete = undefined
    | otherwise = undefined
modelUpdate _ _ _ = return $ EmptyResult notFound404

buildRequest :: Path -> Method -> BS.ByteString -> String -> Request
buildRequest path method body rootUrl = fromJust (parseUrl (rootUrl ++ (show path)))

genAction :: Model -> Gen Action
genAction model = do
    path <- genPath model
    method <- genMethod path
    body <- genBody path method
    let bodyValue = decodeStrict body
    return $ Action 
        { actionRequest     = buildRequest path method body
        , actionModelUpdate = runState $ modelUpdate path method bodyValue
        }
    --[ symptomsR, symptomR, symptomPointsR, badPathR]
    where
        symptomsR = undefined
        symptomR = undefined
        symptomPointsR = undefined
        symptomPointR = undefined
        badPathR = undefined

genActionSeq :: Gen (Seq Action)
genActionSeq = sized $ \n ->
    do k <- choose (0,n)
       genActionVector k

initialModel :: Model
initialModel = Model Map.empty 0 0

genActionVector :: Int -> Gen (Seq Action)
genActionVector length = do
        (seq,_) <- go length
        return seq
    where
        go 0 = return (Seq.empty,initialModel)
        go n = do
            (subSeq,model) <- go (n-1)
            act <- genAction model
            let (_,model') = actionModelUpdate act model
            return (subSeq |> act, model')

shrinkActionSeq :: Seq Action -> [Seq Action]
shrinkActionSeq seq = go Seq.empty seq
    where
        go seq1 (viewl -> EmptyL) = []
        go seq1 (viewl -> act :< seq2) = (seq1 >< seq2) : go (seq1 |> act) seq2

instance Arbitrary (Seq Action) where
    arbitrary = genActionSeq
    shrink = shrinkActionSeq

prop_model :: Seq Action -> Property
prop_model actions = monadicIO $ do
        serverThread <- run $ forkIO $ runApp ":memory:"
        port <- run $ getEnv "PORT"
        let rootUrl = "http://localhost:"++(show port)
            go model (viewl -> EmptyL) = return ()
            go model (viewl -> a0 :< actions') = do
                actualResult <- run $ performAction rootUrl a0
                let (expectedResult,model') = actionModelUpdate a0 model
                assert (actualResult == expectedResult)
                go model' actions'
        go initialModel actions