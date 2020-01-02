-- tests/GettingStartedSpec.hs
-- module QuickCheckSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import QuickCheck.ArbitraryTypes
import QuickCheck.TestValidTypes

spec :: Spec
spec = do
    describe "QuickCheck" $ do
        prop "prop_bisimilar" $
          verboseCheckWith stdArgs {maxSuccess = 271, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar
          -- verboseCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar


main = hspec spec
