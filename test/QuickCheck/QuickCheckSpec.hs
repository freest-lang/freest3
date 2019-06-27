-- tests/GettingStartedSpec.hs
-- module QuickCheckSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import ArbitraryTypes
import TestValidTypes

spec :: Spec
spec = do
    describe "QuickCheck" $ do
        prop "prop_bisimilar" $
          quickCheckWith stdArgs {maxSuccess = 40000} prop_bisimilar


main = hspec spec
