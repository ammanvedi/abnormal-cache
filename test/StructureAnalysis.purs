module Test.StructureAnalysis where

import Data.Abnormal.StructureAnalysis (addOperationResultToGraph, createGraph)
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
            it "Generates flattened representation correctly" do
                case testJsonSimple of
                    Left s -> shouldEqual true false
                    Right j ->
                        let
                            res = addOperationResultToGraph createGraph "testOperation" j
                        in
                            shouldEqual res createGraph