module Equivalence.TestBisimInvalidSpec (spec) where

import           Bisimulation.Bisimulation (bisimilar)
import           Validation.Rename
import           SpecHelper

-- Note that the tests cases should be kinded!

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [t, u] =
  it (t ++ " ~ " ++  u) (
      {-# SCC "BISIM_TEST_CALL" #-}
      bisimilar t' u' `shouldBe` False)
    where
      [t', u'] = renameTypes [read t, read u]
      -- readKenv :: String -> KindEnv
      -- readKenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestBisimInvalid.txt"
  describe "Invalid Bissim Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec

