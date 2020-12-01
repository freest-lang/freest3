module Validation.TestTypesValidSpec (spec) where

import           Syntax.Kinds (KindEnv, Kind, (<:))
import           Validation.Kinding (synthetise)
import           Utils.FreestState (initialState, errors)
import           SpecHelper
import           Control.Monad.State

spec :: Spec
spec =
  -- t <- runIO $ readFromFile "test/UnitTests/Validation/TestContractivityValid.txt"
  describe "Valid types tests" $ do
    t <- runIO $ readFromFile "test/UnitTests/Validation/TestTypesValid.txt"
    mapM_ matchValidKindingSpec (chunksOf 3 t)

matchValidKindingSpec :: [String] -> Spec
matchValidKindingSpec [kEnv, t, k] =
  it t $ hasKind (readKenv kEnv) (read t) (read k) `shouldBe` True
  
-- code from QuickCheck
kindOf ::  KindEnv -> Type -> Maybe Kind
kindOf kEnv t
  | null (errors s) = Just k
  | otherwise       = Nothing
  where (k, s) = runState (synthetise kEnv t) (initialState "Kind syntesis")

hasKind :: KindEnv -> Type -> Kind -> Bool
hasKind kEnv t k = case kindOf kEnv t of
  Nothing -> False
  Just k' -> k' <: k

-- INVALID:
-- forall alpha . (rec Tree . &{Leaf:Skip, Node:?Int;Tree;Tree}) -> (rec TreeChannel . +{Leaf:Skip, Node:!Int;TreeChannel;TreeChannel});alpha->alpha

main :: IO ()
main = hspec spec
