module Main where

import Compiler
import System.Environment
import System.Process
import System.Exit


main :: IO ()
main = do
  args <- getArgs
  res <- compile (head args)
  if res then
    do 
      (exitcode, output, errors) <- readProcessWithExitCode "ghc" ["cfst.hs"] ""
      (exitcode1, output1, errors1) <- readProcessWithExitCode "./cfst" [] ""
      exitSuccess
  else
    exitFailure
