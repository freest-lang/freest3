import GrammarParser
import Bisimulation.Bisimulation
import Bisimulation.Grammar
import System.Environment (getArgs)
import Prelude hiding (Word) -- Word is (re)defined in module Equivalence.Grammar


main :: IO ()
main = do
  putStrLn "Provide a grammar (Ctrl+d to terminate):"
  contents <- getContents
  g <- parseGrammar contents
  isBisimGrammar g

isBisimGrammar :: Grammar -> IO ()
isBisimGrammar g@(Grammar [xs, ys] _)
  | bisimilarGrm g =
      putStrLn $ "Words " ++ showWord xs ++
                 "and " ++ showWord ys ++ "are bisimilar"
  | otherwise          =    
      putStrLn $ "Words " ++ showWord xs ++
                 "and " ++ showWord ys ++ "are *not* bisimilar"

showWord :: Word -> String
showWord = foldr (\w acc -> show w ++ " " ++ acc) ""
