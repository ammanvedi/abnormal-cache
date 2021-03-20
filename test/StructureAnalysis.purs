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

testJsonUncacheable :: Either String Json
testJsonUncacheable = jsonParser
        """
        {
            "strVal": "val",
            "numVal": 1,
            "boolVal": false,
            "arrVal": [
                "arrStr",
                true,
                567,
                [
                    {
                        "nestedArrayVal": 100
                    }
                ]
            ],
            "objVal": {
                "strValNested": "val",
                "numValNested": 1,
                "boolValNested": false,
                "arrValNested": [
                    "arrStrNested",
                    true,
                    567
                ]
            }
        }
        """

testJsonCacheable :: Either String Json
testJsonCacheable = jsonParser
        """
        {
            "id": "ONE",
            "strVal": "val",
            "numVal": 1,
            "boolVal": false,
            "arrVal": [
                "arrStr",
                true,
                567,
                [
                    {
                        "id": "TWO",
                        "nestedArrayVal": 100
                    }
                ]
            ],
            "objVal": {
                "id": "THREE",
                "strValNested": "val",
                "numValNested": 1,
                "boolValNested": false,
                "arrValNested": [
                    "arrStrNested",
                    true,
                    567
                ]
            }
        }
        """

testJsonMultipleOfSameCacheable :: Either String Json
testJsonMultipleOfSameCacheable = jsonParser
        """
        {
            "child": {
                "id": "ONE",
                "data": "X"
            },
            "childTwo": {
                "id": "ONE",
                "data": "X"
            }
        }
        """

testJsonRootArray :: Either String Json
testJsonRootArray = jsonParser
        """
        [
            "arrStr",
            true,
            567,
            [
                {
                    "id": "TWO",
                    "nestedArrayVal": 100
                }
            ]
        ]
        """

testJsonUpdateCacheStart :: Either String Json
testJsonUpdateCacheStart = jsonParser
        """
        {
            "id": "1234-4567",
            "name": "Benny",
            "age": 10
        }
        """

testJsonUpdateCacheEnd :: Either String Json
testJsonUpdateCacheEnd = jsonParser
        """
        {
            "id": "1234-4567",
            "name": "Johnny",
            "age": 10
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