module Types.TestKindingValidSpec(spec) where

-- import System.IO
-- import Control.Monad
-- import Data.List
-- import Data.Char
import SpecHelper
import Types.Kinds
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/Types/TestContractivityValid.txt"
  describe "Valid Contractivity Test" $ do
      mapM_ matchValidSpec (convert t)
  describe "Valid kinding tests" $ do
    t <- runIO $ readFromFile "test/Types/TestKindingValid.txt"
    mapM_ matchValidKindingSpec (convert t)

matchValidSpec :: (String, String) -> Spec
matchValidSpec (a, b) =
  it a $ do
    (contractive Map.empty (read a :: Type)) `shouldBe` (read b :: Bool)

matchValidKindingSpec :: (String, String) -> Spec
matchValidKindingSpec (a, b) =
  it a $ do
    (kindOf (read a :: Type)) `shouldBe` (read b :: Kind)

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha
