module Main.GrammarBisimulation where

import Equivalence.Bisimulation
import Parse.GrammarParser
import Equivalence.Grammar
import System.Environment (getArgs)
import Prelude hiding (Word) -- Word is (re)defined in module Equivalence.Grammar


main :: IO ()
main = do
  -- args <- getArgs
  -- f <- readFile (head args)
  contents <- getContents
  g <- parseGrammar contents
  isBisimGrammar g

isBisimGrammar :: Grammar -> IO ()
isBisimGrammar g@(Grammar [xs, ys] _)
  | bisimilar g =
      putStrLn $ "Words " ++ showWord xs ++
                 "and " ++ showWord ys ++ "are bisimilar"
  | otherwise          =    
      putStrLn $ "Words " ++ showWord xs ++
                 "and " ++ showWord ys ++ "are *not* bisimilar"


showWord :: Word -> String
showWord = foldr (\w acc -> show w ++ " " ++ acc) ""
