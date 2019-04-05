module Validation.TestKindingInvalidSpec(spec) where

import           Validation.Kinding
import           Syntax.Kinds
import           Utils.FreestState
import           SpecHelper
import           Control.Monad.State
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Invalid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestKindingInvalid.txt"
    mapM_ matchInvalidKindingSpec t

matchInvalidKindingSpec :: String -> Spec
matchInvalidKindingSpec str =
  it str $ do
    (read str `isWellFormed` Map.empty) `shouldBe` False

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let s = initialState "" in
  let s1 = execState (synthetise Map.empty t) s in -- (s {kindEnv=k}) in
    null (errors s1)

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec


