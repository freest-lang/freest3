module Main where

import Compiler
import System.Environment
import System.Process
import System.Exit
import System.Directory


main :: IO ()
main = do
  args <- getArgs
  res <- compile (head args)
  let filepath = reverse $ dropWhile (/= '/') (reverse (head args))
  checkResult res filepath


checkResult :: (Bool, String) -> String -> IO ()
checkResult res filepath = do
  setCurrentDirectory filepath
  case res of
    (True, _) -> do
      (exitcode, output, errors) <- readProcessWithExitCode "ghc" ["cfst.hs"] ""
      (exitcode1, output1, errors1) <- readProcessWithExitCode "./cfst" [] ""
      putStr output1
      exitSuccess
    (False, err) -> do
      putStrLn err
      exitFailure
  
    
