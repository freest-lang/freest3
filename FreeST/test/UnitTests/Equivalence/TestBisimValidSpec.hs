module Equivalence.TestBisimValidSpec (spec) where

import           Bisimulation.Bisimulation (bisimilar)
import           Validation.Rename
import           SpecHelper

-- Note that the tests cases should be kinded!

matchValidSpec :: [String] -> Spec
matchValidSpec [t, u] =
  it (t ++ " ~ " ++  u) (
      {-# SCC "BISSIM_TEST_CALL" #-}
      bisimilar t' u' `shouldBe` True)
    where
      [t', u'] = renameTypes [read t, read u]

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestBisimValid.txt"
  describe "Valid Bissim Test" $ mapM_ matchValidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
