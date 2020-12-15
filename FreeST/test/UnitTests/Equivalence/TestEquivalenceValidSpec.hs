module Equivalence.TestEquivalenceValidSpec (spec) where

import           Syntax.Base
import           Syntax.Kind
import           Equivalence.Equivalence
import           Validation.Rename
import qualified Data.Map.Strict as Map
import           SpecHelper

-- Note that the tests cases should be kinded!

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] = it
  (k ++ "  |-  " ++ t ++ " ~ " ++  u)
  (equivalent (readKenv k) t' u' `shouldBe` True)
  where
    [t', u'] = renameTypes [read t, read u]
    -- readKenv :: String -> KindEnv
    -- readKenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
  tests <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 tests)

main :: IO ()
main = hspec spec

