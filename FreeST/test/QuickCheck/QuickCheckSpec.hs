import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import TestValidTypes

args = stdArgs {maxSuccess = 150135, replay = Just (mkQCGen 1095646480, 0)}
-- args = stdArgs {maxSuccess = 120135, replay = Just (mkQCGen 1095646480, 0)}
-- agrs = stdArgs {maxSuccess = 108375, replay = Just (mkQCGen 1095646480, 0)}

spec :: Spec
spec =
  describe "QuickCheck" $
    prop "prop_bisimilar" $
      verboseCheckWith args prop_distribution -- prop_bisimilar

main = hspec spec
