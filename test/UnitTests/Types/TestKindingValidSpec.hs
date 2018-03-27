module UnitTests.Types.TestKindingValidSpec(spec) where

import           SpecHelper
import           Types.Kinds
import           Types.Kinding
import qualified Data.Map.Strict as Map
-- import           System.Log.Logger
import Control.Monad.Writer


-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestContractivityValid.txt"

  -- describe "Valid Contractivity Test" $ do
  --   mapM_ matchValidSpec (convert t)
  describe "Valid kinding tests" $ do

    t <- runIO $ readFromFile "test/UnitTests/Types/TestKindingValid.txt"
    mapM_ matchValidKindingSpec (convert t)

-- matchValidSpec :: (String, String) -> Spec
-- matchValidSpec (a, b) =
--   it a $ do
--     (contractive Map.empty (read a :: Type)) `shouldBe` (read b :: Bool)

matchValidKindingSpec :: (String, String) -> Spec
matchValidKindingSpec (a, b) =
  it a $ do
    (kindOf (read a :: Type)) `shouldBe` (read b :: Kind)


-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha
