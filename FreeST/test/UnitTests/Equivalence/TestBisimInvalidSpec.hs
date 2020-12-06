module Equivalence.TestBisimInvalidSpec (spec) where

-- import           Syntax.Type
-- import           Syntax.Kind
-- import           Syntax.TypeVariables
-- import           Syntax.Base
import           Equivalence.Equivalence (bisimilar)
import           Validation.Rename
-- import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           SpecHelper

-- Note that the tests cases should be kinded!

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [t, u] =
  it (t ++ " ~ " ++  u) (
      {-# SCC "BISIM_TEST_CALL" #-}
      bisimilar Map.empty t' u' `shouldBe` False)
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

