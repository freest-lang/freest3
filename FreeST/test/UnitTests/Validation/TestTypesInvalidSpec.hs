module Validation.TestTypesInvalidSpec(spec) where

import           Validation.Kinding
import           Syntax.Kinds
import           Utils.FreestState
import           SpecHelper
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Base (defaultPos)
-- import           Data.Either

spec :: Spec
spec = do
  describe "Invalid types tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesInvalid.txt"
    mapM_ matchInvalidKindingSpec t

matchInvalidKindingSpec :: String -> Spec
matchInvalidKindingSpec str =
  it str $ do
    (str `isWellFormed` Map.empty) `shouldBe` False

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- If the value is Left a, apply the first function to a;
-- if it is Right b, apply the second function to b.

isWellFormed :: String -> KindEnv -> Bool
isWellFormed str _ =
  let t = parseType str in
    either synthetiseK (\_ -> False) t
  where
    synthetiseK t =
      let s  = initialState ""
          s1 = execState (synthetise Map.empty t) s in
          null (errors s1)

-- isWellFormed :: Type -> KindEnv -> Bool
-- isWellFormed t k =
--   let s = initialState "" in
--   let s1 = execState (synthetise Map.empty t) s in -- (s {kindEnv=k}) in
--     null (errors s1)

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec


