module Validation.TestKindingValidSpec(spec) where

import SpecHelper
import Validation.Contractive
import Validation.Kinding
--
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestContractivityValid.txt"
  describe "Valid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestKindingValid.txt"
    mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [a, b] =
  it a $
    (kindOf Map.empty (read a)) `shouldBe` (read b)

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec
