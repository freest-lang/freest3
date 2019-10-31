module Main where

-- import System.Exit
import Equivalence.Bisimulation
import Parse.GrammarParser
import Equivalence.Grammar
-- import Syntax.Types
-- import Syntax.Kinds
-- import Utils.FreestState
-- import Control.Monad.State
-- import Validation.Kinding
-- import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import           Prelude hiding (Word) -- Word is (re)defined in module Equivalence.Grammar


main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  let g = parseGrammars f
  isBisimGrammar (head g)

isBisimGrammar :: Grammar -> IO ()
isBisimGrammar g@(Grammar [xs, ys] _)
  | bisimilarGrammar g =
      putStrLn $ "The languages generated by " ++ showWord xs ++
                 " and " ++ showWord ys ++ " are bisimilar"
  | otherwise          =    
      putStrLn $ "The languages generated by " ++ showWord xs ++
                 " and " ++ showWord ys ++ " are *not* bisimilar"


showWord :: Word -> String
showWord xs = concat $ map show xs
