module Equivalence.TestEquivalenceValidSpec (spec) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Equivalence.Equivalence
import           Validation.Rename
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           SpecHelper

-- Note that the tests cases should be kinded!

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) (
      {-# SCC "EQUIVALENT_TEST_CALL" #-}
      equivalent Map.empty (readKenv k) t' u' `shouldBe` True)
    where [t', u'] = renameList [read t, read u]
          readKenv :: String -> KindEnv
          readKenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)

main :: IO ()
main = hspec spec

