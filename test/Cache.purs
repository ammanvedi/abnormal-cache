module Test.Cache where

import Cache.Abnormal (f)
import Prelude (Unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

cacheSpec :: Spec Unit
cacheSpec =
    describe "Cache" do 
        describe "t" do
            it "returns 0 for an empty array" do
                let res = f "abc"
                res `shouldEqual` "abc"
