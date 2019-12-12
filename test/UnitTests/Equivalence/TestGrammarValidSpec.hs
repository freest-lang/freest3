module Equivalence.TestGrammarValidSpec (spec) where

import           Control.Monad (foldM_)
import           Data.List.Split as L
import qualified Data.Map.Strict as Map
import           Equivalence.Bisimulation (bisimilarGrammar)
import           Equivalence.Grammar
import           Parse.GrammarParser
import           SpecHelper
import           Syntax.Base
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Types
import           Utils.FreestState
import           Validation.Rename
import qualified Control.Monad as M
-- Note that the tests cases should be kinded!

matchValidSpec :: Int -> Grammar -> Spec
matchValidSpec i g =
  it ("Bisimilar grammar test " ++ show i ++ " (~)") (
      bisimilarGrammar g `shouldBe` True)

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestGrammarValid.txt"
--  let gs = parseGrammars (unlines t)
  gs <- runIO $ M.sequence $ map parseGrammar (L.splitOn "|" (unlines t))  
  describe "Valid Bissim Test" $
    foldM_ (\i g -> matchValidSpec i g >> return (i+1)) 1 gs


main :: IO ()
main = hspec spec
