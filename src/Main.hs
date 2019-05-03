module Main where

import Compiler
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    compileFile (head args)
  else
    putStrLn "Error: Incorrect number of arguments, provide just one argument"
