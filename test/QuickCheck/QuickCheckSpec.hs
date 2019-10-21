-- tests/GettingStartedSpec.hs
-- module QuickCheckSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import QuickCheck.ArbitraryTypes
import QuickCheck.TestValidTypes

spec :: Spec
spec = do
    describe "QuickCheck" $ do
        prop "prop_bisimilar" $
          quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar


main = hspec spec
