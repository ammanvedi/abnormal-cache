module Test.Main where

import Test.StructureAnalysis (structureAnalysisSpec)

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($))
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [specReporter] do
    structureAnalysisSpec
