module Subtyping.TestSubInvalidSpec (spec) where

import           Bisimulation.Bisimulation ((<~))
import           Validation.Rename
import           SpecUtils

-- Note that the tests cases should be kinded!

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [t, u] =
  it (t ++ " </~ " ++  u) (
      {-# SCC "BISIM_TEST_CALL" #-}
      (t' <~ u') `shouldBe` False)
    where
      [t', u'] = renameTypes [read t, read u]
      -- readKenv :: String -> KindEnv
      -- readKenv s = Map.fromList $ map (\(x,k) -> (mkVar defaultPos x, k)) (read s)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Subtyping/TestSubInvalid.txt"
  describe "Invalid Sub Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec

