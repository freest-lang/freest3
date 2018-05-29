module Types.TestKindingValidSpec(spec) where

import SpecHelper
import Types.Kinds
import Types.Kinding
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestContractivityValid.txt"
  describe "Valid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestKindingValid.txt"
    mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [a, b] =
  it a $
    (kindOf Map.empty (read a :: Type)) `shouldBe` (read b :: Kind)


-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha
