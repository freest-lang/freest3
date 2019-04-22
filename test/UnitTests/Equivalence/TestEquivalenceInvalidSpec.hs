module Equivalence.TestEquivalenceInvalidSpec(spec) where

import           Equivalence.Equivalence
import           Validation.Rename
import           Syntax.Types
import           Syntax.Base
import           Utils.FreestState
import           SpecHelper
import qualified Data.Map.Strict as Map
import           Control.Monad.State

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [a, b] =
  it (a ++ " `~/~` " ++  b) $
    equivalent Map.empty Map.empty t u `shouldBe` False
          where (PairType _ t u) = evalState (rename Map.empty (PairType defaultPos (read a) (read b))) (initialState "Testing Type Equivalence")

main :: IO ()
main = hspec spec
