module Test.StructureAnalysis where

import Data.Abnormal.StructureAnalysis (addOperationResultToGraph, createGraph)
import Data.Either (Either(..))
import Prelude (Unit, discard)
import Test.Fixture (testJsonSimple, testJsonSimpleResultGraph)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

structureAnalysisSpec :: Spec Unit
structureAnalysisSpec =
    describe "StructureAnalysis" do 
        describe "traverseJson" do
            it "Generates correct graph for a cacheable object at the top level" do
                case testJsonSimple of
                    Left s -> shouldEqual true false
                    Right j ->
                        let
                            res = addOperationResultToGraph createGraph "testOperation" j
                        in
                            shouldEqual res testJsonSimpleResultGraph
            it "Generates correct graph for a cacheable object nested in an uncacheable object" do
                case testJsonSimple of
                    Left s -> shouldEqual true false
                    Right j ->
                        let
                            res = addOperationResultToGraph createGraph "testOperation" j
                        in
                            shouldEqual res testJsonSimpleResultGraph