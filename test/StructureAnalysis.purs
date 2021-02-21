module Test.StructureAnalysis where

import Data.Abnormal.StructureAnalysis (initialContext, normalise)
import Data.Argonaut (Json, jsonParser, jsonZero, stringify, toObject)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Foreign.Object as FO
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testJson :: Either String Json
testJson = jsonParser
        """
        { "a": 1, "b": "str", "c": true }
        """
testJsonComplex :: Either String Json
testJsonComplex = jsonParser
        """
            {
                "data": {
                    "user": {
                        "name": "Amman Vedi",
                        "location": "West London",
                        "age": 100,
                        "verified": true,
                        "metadata": {
                            "likesDogs": true,
                            "externalId": "1234-5678-7890"
                        }
                    }
                }
            }
        """

testJsonSimple :: Either String Json
testJsonSimple = jsonParser
        """
            {
                "id": "parentID",
                "b": "b-value",
                "c": {
                    "id": "childID",
                    "d": false
                },
                "d": [
                    "e",
                    {
                        "id": "arrId",
                        "f": "g"
                    }
                ]
            }

        """

structureAnalysisSpec :: Spec Unit
structureAnalysisSpec =
    describe "StructureAnalysis" do 
        describe "traverseJson" do
            -- it "Generates a basic schema correctly" do
            --     case testJsonSimple of
            --         Left s -> shouldEqual true false
            --         Right j -> do
            --             let emp = FO.empty
            --             let o = FO.insert "dta" j emp
            --             let res = traverseJson o
            --             shouldEqual res []
            -- it "Generates flattened representation correctly" do
            --     case testJsonSimple of
            --         Left s -> shouldEqual true false
            --         Right j -> do
            --             let emp = FO.empty
            --             let o = FO.insert "root" j emp
            --             let schema = traverseJson o
            --             let res = flattenDataObject schema o
            --             shouldEqual res []
            it "Generates flattened representation correctly" do
                case testJsonSimple of
                    Left s -> shouldEqual true false
                    Right j -> do
                        case toObject j of
                            (Just o) -> 
                                let
                                    res = normalise o [] Nothing
                                in
                                shouldEqual res (initialContext FO.empty [])
                            Nothing -> shouldEqual false true