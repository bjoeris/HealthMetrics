module Test where

import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.LocalTime
import Network.HTTP.Client

data SymptomPoint = SymptomPoint
    { symptomPointValue :: Int
    , symptomPointTime :: ZonedTime
    }

data Symptom = Symptom
    { symptomName :: String
    , symptomPoints :: Map String SymptomPoint
    }

data Model = Model
    { modelSymptoms :: Map String Symptom }

data Action = Action
    { actionRequest     :: Request
    , actionModelUpdate :: Model -> Model
    }

arbitraryAction :: String -> Model -> Gen Action
arbitraryAction rootURL model = oneOf 
    [symptoms, symptom, symptomPoints, symptomPoint, badPath]
    where
        symptoms = undefined
        symptom = undefined
        symptomPoints = undefined
        symptomPoint = undefined
        badPath = undefined