import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import ArbitraryTypes
import TestValidTypes

spec :: Spec
spec =
  describe "QuickCheck" $ do
    prop "prop_bisimilar" $
      verboseCheckWith stdArgs {maxSuccess = 108375, replay = Just (mkQCGen 1095646480, 0)} prop_distribution -- prop_bisimilar
      -- verboseCheckWith stdArgs {maxSuccess = 27100, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar -- prop_distribution
      -- verboseCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar

main = hspec spec
