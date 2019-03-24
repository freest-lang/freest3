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

compileFile :: FilePath -> IO ()
compileFile args 
  | "fst" `isExtensionOf` args = do
      res <- compile args -- TODO: review compile name (it's sounds odd)
      checkResult res args
  | otherwise = do
      putStrLn $ "Error: File extension not recognized, provide a .fst file: " ++ args


checkResult :: (Bool, String) -> String -> IO ()
checkResult res filepath = do
  let (path, filename) = splitFileName filepath
  changeDir path
  case res of
    (True, _) -> do -- Join these two (repeated)
      (exitcode, _, errors) <- readProcessWithExitCode "ghc" [targetFileName filename] ""
      checkGhcOut exitcode errors
      
      (exitcode1, output1, errors1) <- readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
      checkGhcOut exitcode1 errors1
      putStr output1
      exitSuccess
    (False, err) -> do
      putStrLn err
      exitFailure
  where
    changeDir :: String -> IO () -- TODO Test
    changeDir d
      | not (null d) = setCurrentDirectory d
      | otherwise    = return ()

targetFileName :: String -> String
targetFileName file = replaceExtensions file "hs"
  
checkGhcOut exitcode errors =
  if (exitcode /= ExitSuccess) then
    do
      putStrLn errors
      exitFailure
  else
    return ()
