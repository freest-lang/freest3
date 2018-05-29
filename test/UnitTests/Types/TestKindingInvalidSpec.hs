module Types.TestKindingInvalidSpec(spec) where

import           SpecHelper
import           Types.Kinds
import           Types.Kinding
import qualified Data.Map.Strict as Map


-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Invalid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Types/TestKindingInvalid.txt"
    mapM_ matchInvalidKindingSpec t

matchInvalidKindingSpec :: String -> Spec
matchInvalidKindingSpec str =
  it str $ do
    (isWellKinded Map.empty (read str :: Type)) `shouldBe` False


-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha
