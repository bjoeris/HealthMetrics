{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Data.Maybe

import Main (runApp)

data ModelSymptomPoint = ModelSymptomPoint
    { symptomPointValue :: Int
    , symptomPointTime :: ZonedTime
    }
    deriving (Show)

data ModelSymptom = ModelSymptom
    { symptomName :: String
    , symptomPoints :: Map String ModelSymptomPoint
    }
    deriving (Show)

data Model = Model
    { modelSymptoms :: Map String ModelSymptom }
    deriving (Show)

data ActionResult
    = EmptyResult Status
    | JSONResult Status Value
    | JSONError Status String
    deriving (Eq,Show)

data Action = Action
    { actionRequest     :: String -> Request IO
    , actionModelUpdate :: Model -> Model
    , actionResult      :: Model -> ActionResult
    }
instance Show Action where
    show act = show (actionRequest act "<root>")

performAction :: String -> Action -> IO ActionResult
performAction rootUrl act = do
    let request' = actionRequest act rootUrl
        request = request { checkStatus = \_ _ _ -> Nothing }
    response <- withManager $ httpLbs request
    let status = responseStatus response
    let rawBody = BS.concat (LBS.toChunks (responseBody response))
    if BS.null rawBody then
        return $ EmptyResult status
    else
        case eitherResult (parse json rawBody) of
            Left errorString -> return $ JSONError status errorString
            Right value -> return $ JSONResult status value

data Path
    = Root
    | Symptoms
    | Symptom String
    | SymptomPoints String
    | SymptomPoint String String
    | BadPath Path String


alphaNumChar :: Gen Char
alphaNumChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

alphaNumString :: Gen String
alphaNumString = listOf alphaNumChar

genSymptomId :: Model -> Gen String
genSymptomId model = if Map.null (modelSymptoms model) then newSymptomId
                     else oneof [ existingSymptomId, newSymptomId ]
    where
        existingSymptomId = elements (Map.keys (modelSymptoms model))
        newSymptomId = suchThat alphaNumString
            (\k -> Map.notMember k (modelSymptoms model))

genSymptomPointId :: Model -> String -> Gen String
genSymptomPointId model symptomId = case Map.lookup symptomId (modelSymptoms model) of
    Nothing -> alphaNumString
    (Just symptom) ->
        let existingSymptomPointId = elements (Map.keys (symptomPoints symptom))
            newSymptomPointId = suchThat alphaNumString (\k -> Map.notMember k (symptomPoints symptom))
        in if Map.null (modelSymptoms model) then newSymptomPointId
           else oneof [ existingSymptomPointId, newSymptomPointId ]

genPath :: Model -> Gen Path
genPath model = oneof
    [ {-return Root
    ,-} return Symptoms
    , symptom
    , symptomPoints
    , symptomPoint
    , badPath ]
    where
        symptom = liftM Symptom (genSymptomId model)
        symptomPoints = liftM SymptomPoints (genSymptomId model)
        symptomPoint = do
            symptomId <- genSymptomId model
            symptomPointId <- genSymptomPointId model symptomId
            return $ SymptomPoint symptomId symptomPointId
        badPath = oneof [ badRootPath, badSymptomPath, badSymptomPointPath ]
        badRootPath         = do
            str <- suchThat alphaNumString (/= "symptoms")
            return $ BadPath Root str
        badSymptomPath      = do
            str       <- suchThat alphaNumString (/= "points")
            symptomId <- genSymptomId model
            return $ BadPath (Symptom symptomId) str
        badSymptomPointPath = do
            str            <- alphaNumString
            symptomId      <- genSymptomId model
            symptomPointId <- genSymptomPointId model symptomId
            return $ BadPath (SymptomPoint symptomId symptomPointId) str

instance Show Path where
    show Root = "/"
    show Symptoms = "/symptoms"
    show (Symptom symptomId) = "/symptoms/" ++ symptomId
    show (SymptomPoints symptomId) = "/symptoms/" ++ symptomId ++ "/points"
    show (SymptomPoint symptomId symptomPointId) = "/symptoms/" ++ symptomId ++ "/points/" ++ symptomPointId
    show (BadPath prefix suffix) = (show prefix) ++ "/" ++ suffix

genMethod :: Path -> Gen Method
genMethod Symptoms = oneof
    [ elements [ methodGet, methodPost ]
    , elements [ methodPut, methodDelete, methodHead ]]
genMethod (Symptom symptomId) = oneof
    [ elements [ methodGet, methodPut, methodDelete ]
    , elements [ methodPost, methodHead ]]
genMethod (SymptomPoints symptomId) = oneof
    [ elements [ methodGet, methodPost ]
    , elements [ methodPut, methodDelete, methodHead ]]
genMethod (SymptomPoint symptomId symptomPointId) = oneof
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

modelUpdate :: Path -> Method -> Model -> (Model,ActionResult)
modelUpdate Symptoms method model
    | method == methodGet  = (model, JSONResult ok200 (
        object ["symptoms" .= undefined]))
    | method == methodPost = undefined
    | otherwise            = undefined

genAction :: Model -> Gen Action
genAction model = do
    path <- genPath model
    method <- genMethod path
    body <- genBody path method
    let request     rootUrl = fromJust (parseUrl (rootUrl ++ (show path)))
        modelUpdate model   = undefined
        result      model   = undefined
    return $ Action 
        { actionRequest     = request
        , actionModelUpdate = modelUpdate
        , actionResult      = result
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
initialModel = Model Map.empty

genActionVector :: Int -> Gen (Seq Action)
genActionVector length = do
        (seq,_) <- go length
        return seq
    where
        go 0 = return (Seq.empty,initialModel)
        go n = do
            (subSeq,model) <- go (n-1)
            act <- genAction model
            let model' = actionModelUpdate act model
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
        let rootUrl = "http://localhost:3000"
            go model (viewl -> EmptyL) = return ()
            go model (viewl -> a0 :< actions') = do
                actualResult <- run $ performAction rootUrl a0
                let expectedResult = actionResult a0 model
                    model' = actionModelUpdate a0 model
                assert (actualResult == expectedResult)
                go model' actions'
        go initialModel actions