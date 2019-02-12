module Main where

import Compiler
import System.Environment
import System.Process
import System.Exit
import System.Directory
import System.FilePath


main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    compileFile (head args)
  else
    putStrLn "Error: Incorrect number of arguments, provide just one argument"


compileFile args
  | takeExtension args == ".cfs" =
    do
      res <- compile args
      let filepath = reverse $ dropWhile (/= '/') (reverse args)
      checkResult res filepath
  | otherwise = do
      putStrLn "Error: File extension not recognized, provide a .cfs file"


checkResult :: (Bool, String) -> String -> IO ()
checkResult res filepath = do
  changeDir filepath   
  case res of
    (True, _) -> do
      (exitcode, _, errors) <- readProcessWithExitCode "ghc" ["cfst.hs"] ""
      checkGhcOut exitcode errors
      (exitcode1, output1, errors1) <- readProcessWithExitCode "./cfst" [] ""
      checkGhcOut exitcode1 errors1
      putStr output1
      exitSuccess
    (False, err) -> do
      putStrLn err
      exitFailure
  where
    changeDir :: String -> IO ()
    changeDir d
      | not (null d) = setCurrentDirectory d
      | otherwise    = return ()
    
  
checkGhcOut exitcode errors =
  if (exitcode /= ExitSuccess) then
    do
      putStrLn errors
      exitFailure
  else
    return ()
