module Validation.TestKindingValidSpec(spec) where

import           Syntax.Kinds
import           Validation.Contractive
import           Validation.Kinding
import           Utils.FreestState
import           SpecHelper
import           Control.Monad.State
import qualified Data.Map.Strict as Map

kindOfType :: KindEnv -> Type -> Kind
kindOfType kenv t = evalState (synthetise kenv t) (initialState  "")
  -- let s = (initialState  "") in
  -- evalState (synthetise kenv t) (s {kindEnv = k})

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Validation/TestContractivityValid.txt"
  describe "Valid kinding tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestKindingValid.txt"
    mapM_ matchValidKindingSpec (chunksOf 2 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [a, b] =
  it a $
    kindOfType Map.empty (read a) <: read b `shouldBe` True

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec
