module Validation.TestKindingInvalidSpec(spec) where

import SpecHelper
import Validation.Kinding
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Invalid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestKindingInvalid.txt"
    mapM_ matchInvalidKindingSpec t

matchInvalidKindingSpec :: String -> Spec
matchInvalidKindingSpec str =
  it str $ do
    (isWellKinded Map.empty (read str)) `shouldBe` False

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec
