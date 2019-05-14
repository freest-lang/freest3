module Equivalence.TestEquivalenceValidSpec(spec) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Equivalence.Equivalence
import           Validation.Rename
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           Control.Monad.State
import           SpecHelper

import           Data.Maybe
import           Validation.Kinding

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) (
    let kenv = Map.fromList (readKenv k) in
      {-# SCC "EQUIVALENT_TEST_CALL" #-}
      -- equivalent Map.empty kenv (read t :: Type) (read u :: Type) `shouldBe` True)
      equivalent Map.empty kenv t' u' `shouldBe` True)
      where [t', u'] = evalState (renameList Map.empty [read t, read u]) (initialState "Testing Type Equivalence")

readKenv :: String -> [(TypeVar, Kind)]
readKenv s = map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

main :: IO ()
main = hspec spec

