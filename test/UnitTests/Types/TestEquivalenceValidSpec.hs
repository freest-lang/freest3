module Types.TestEquivalenceValidSpec(spec) where

import qualified Data.Map.Strict as Map
import SpecHelper
import Types.Kinds
import Types.Kinding
import Types.TypeEquivalence

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (convert3 t)

matchValidSpec :: (String, String, String) -> Spec
matchValidSpec (k, t, u) =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) $
    equivalent (Map.fromList (read k)) (read t) (read u) `shouldBe` True
