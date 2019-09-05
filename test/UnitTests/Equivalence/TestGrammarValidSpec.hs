module Equivalence.TestGrammarValidSpec (spec) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Equivalence.Bisimulation (bisimilarGrammar)
import           Equivalence.GrammarParser
import           Equivalence.Grammar
import           Validation.Rename
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           SpecHelper
import           Control.Monad (foldM_)

-- Note that the tests cases should be kinded!

matchValidSpec :: Int -> Grammar -> Spec
matchValidSpec i g =
  it ("Bisimilar grammar test " ++ show i ++ " (~)") (
      bisimilarGrammar g `shouldBe` True)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestGrammarValid.txt"
  let gs = parseGrammars (unlines t)
  describe "Valid Bissim Test" $ foldM_ (\i g -> matchValidSpec i g >> return (i+1)) 1 gs


main :: IO ()
main = hspec spec
