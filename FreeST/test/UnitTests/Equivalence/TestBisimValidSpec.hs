module Equivalence.TestBisimValidSpec (spec) where

import           Bisimulation.Bisimulation (bisimilar)
import           Validation.Rename
import           SpecUtils

-- Note that the tests cases should be kinded! but not necessarily renamed

matchValidSpec :: [String] -> Spec
matchValidSpec [s1, s2] =
  it (show t ++ " ~ " ++  show u) (
      {-# SCC "BISIM_TEST_CALL" #-}
      bisimilar t' u' `shouldBe` True)
    where
      t = read s1
      u = read s2
      [t', u'] = renameTypes [t, u]

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestBisimValid.txt"
  describe "Valid Bisim Test" $ mapM_ matchValidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
