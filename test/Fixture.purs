module Test.Fixture where

import Data.Abnormal.Graph (Edge(..), EdgeConnection(..), EdgeData(..), Graph(..), Node(..), NodeData(..))
import Data.Argonaut (Json, jsonParser)
import Data.Either (Either)
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))

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

testJsonSimple :: Either String Json
testJsonSimple = jsonParser
        """
            {
                "id": "parentID",
                "b": "b-value",
                "c": 100,
                "d": false,
                "e": null
            }
        """

testJsonSimpleResultGraph :: Graph
testJsonSimpleResultGraph = 
    Graph 
        (fromFoldable
            [
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]" 
                        CachedObject
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b" 
                        (ValueString "b-value")
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c" 
                        (ValueNumber 100.0)
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d" 
                        (ValueBoolean false)
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e" 
                        ValueNull
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id" 
                        (ValueString "parentID")
                    ),
                Tuple 
                    "testOperation" 
                    (Node "testOperation" 
                        CachedOperation
                    )
            ]
        )
        [
            (
                Edge "testOperation_parentID::[id:st][b:st][c:nm][d:bl][e:null]_root"
                    (EdgeConnection "testOperation" "parentID::[id:st][b:st][c:nm][d:bl][e:null]")
                    (PropertyEdge "root")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_id_id"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id")
                    (PropertyEdge "id")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_b_b"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b")
                    (PropertyEdge "b")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_c_c"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c")
                    (PropertyEdge "c")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_d_d"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d")
                    (PropertyEdge "d")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_e_e"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e")
                    (PropertyEdge "e")
            )
        ]

testJsonSimpleUncacheable :: Either String Json
testJsonSimpleUncacheable = jsonParser
        """
            {
                "id": "parentID",
                "b": "b-value",
                "c": 100,
                "d": false,
                "e": null
            }
        """

testJsonSimpleResultGraphUncacheable :: Graph
testJsonSimpleResultGraphUncacheable = 
    Graph 
        (fromFoldable
            [
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]" 
                        CachedObject
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b" 
                        (ValueString "b-value")
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c" 
                        (ValueNumber 100.0)
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d" 
                        (ValueBoolean false)
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e" 
                        ValueNull
                    ),
                Tuple 
                    "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id" 
                    (Node "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id" 
                        (ValueString "parentID")
                    ),
                Tuple 
                    "testOperation" 
                    (Node "testOperation" 
                        CachedOperation
                    )
            ]
        )
        [
            (
                Edge "testOperation_parentID::[id:st][b:st][c:nm][d:bl][e:null]_root"
                    (EdgeConnection "testOperation" "parentID::[id:st][b:st][c:nm][d:bl][e:null]")
                    (PropertyEdge "root")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_id_id"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_id")
                    (PropertyEdge "id")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_b_b"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_b")
                    (PropertyEdge "b")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_c_c"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_c")
                    (PropertyEdge "c")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_d_d"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_d")
                    (PropertyEdge "d")
            ),
            (
                Edge "parentID::[id:st][b:st][c:nm][d:bl][e:null]_parentID::[id:st][b:st][c:nm][d:bl][e:null]_e_e"
                    (EdgeConnection "parentID::[id:st][b:st][c:nm][d:bl][e:null]" "parentID::[id:st][b:st][c:nm][d:bl][e:null]_e")
                    (PropertyEdge "e")
            )
        ]
