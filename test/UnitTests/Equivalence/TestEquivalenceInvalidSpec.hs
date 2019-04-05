module Equivalence.TestEquivalenceInvalidSpec(spec) where

import           Equivalence.Equivalence
import           Syntax.Types
import qualified Data.Map.Strict as Map
import           SpecHelper

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [a, b] =
  it (a ++ " `equivalent` " ++  b) $
    equivalent Map.empty Map.empty (read a :: Type) (read b :: Type) `shouldBe` False

main :: IO ()
main = hspec spec
