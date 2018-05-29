module Types.TestEquivalenceValidSpec(spec) where

import SpecHelper
import Types.TypeEquivalence
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) $
    equivalent (Map.fromList (read k)) (read t) (read u) `shouldBe` True

main :: IO ()
main = hspec spec
