module Equivalence.SingleTestSpec where

import           Bisimulation.Bisimulation (bisimilar)
import           Validation.Rename
import           SpecUtils

matchSpec :: [String] -> Spec
matchSpec [t, u, r] =
  it (t ++ " ~ " ++  u) (
      {-# SCC "BISIM_TEST_CALL" #-}
      bisimilar t' u' `shouldBe` read r)
    where
      [t', u'] = renameTypes [read t, read u]
      -- readKenv :: String -> KindEnv
      -- readKenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/SingleTest.txt"
  describe "Test" $ mapM_ matchSpec $ chunksOf 3 t


main :: IO ()
main = hspec spec