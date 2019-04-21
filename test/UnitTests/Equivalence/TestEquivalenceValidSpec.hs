module Equivalence.TestEquivalenceValidSpec(spec) where

import           Equivalence.Equivalence
import           Validation.Rename
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           SpecHelper
import qualified Data.Map.Strict as Map
import           Control.Monad.State
import           Utils.FreestState

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  -- runIO $ error $ show $ chunksOf 3 t
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) (
    {-# SCC "EQUIVALENT_TEST_CALL" #-} equivalent Map.empty (Map.fromList (readKenv k)) t' u' `shouldBe` True)
      where (PairType _ t' u') = evalState (rename Map.empty (PairType defaultPos (read t) (read u))) (initialState "Testing Type Equivalence")

--  :: [(String,Kind)]
readKenv s =
  map (\(x,k) -> (mkVar defaultPos x, k)) xs
  where xs = read s :: [(String,Kind)]
  
main :: IO ()
main = hspec spec

