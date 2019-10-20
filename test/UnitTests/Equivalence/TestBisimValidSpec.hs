module Equivalence.TestBisimValidSpec (spec) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Equivalence.Bisimulation (bisimilar)
import           Validation.Rename
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           SpecHelper
import Debug.Trace

-- Note that the tests cases should be kinded!

matchValidSpec :: [String] -> Spec
matchValidSpec [t, u] =
  it (t ++ " ~ " ++  u) (
      {-# SCC "BISSIM_TEST_CALL" #-}
      bisimilar Map.empty t' u' `shouldBe` True)
    where
      [t', u'] = renameTypes [read t, read u]
      -- readTenv :: String -> TypeEnv
      -- readTenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
--  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestBisimValid.txt"
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestQuickCheck.txt"
  describe "Valid Bissim Test" $ mapM_ matchValidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
