module Types.TestEquivalenceInvalidSpec(spec) where

import qualified Data.Map.Strict as Map
import           SpecHelper
import           Types.TypeEquivalence

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Types/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $
      mapM_ matchInvalidSpec (chunksOf 2 t)


matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [a, b] =
  it (a ++ " `equivalent` " ++  b) $
    equivalent Map.empty (read a) (read b) `shouldBe` False
