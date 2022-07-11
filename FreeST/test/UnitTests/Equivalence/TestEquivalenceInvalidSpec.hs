module Equivalence.TestEquivalenceInvalidSpec
  ( spec
  )
where

import           Equivalence.Equivalence        ( equivalent )
import           Validation.Rename
import           Syntax.Type
import           Util.FreestState
import           SpecUtils
import qualified Data.Map.Strict               as Map
import           Control.Monad.State

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [a, b] =
  it (a ++ " `~/~` " ++ b) $ equivalent Map.empty t u `shouldBe` False
 where
  (Pair p t u) =
    evalState (rename Map.empty Map.empty (Pair p (read a) (read b))) initialState

spec :: Spec
spec = do
  t <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceInvalid.txt"
  describe "Invalid Equivalence Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
