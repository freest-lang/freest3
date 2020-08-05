module SpecUtils where

import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeExtension)
import Test.Hspec (Spec, runIO, describe)

getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
  | otherwise = getSource xs

specTest :: String -> String -> (String -> String -> Spec) -> Spec
specTest desc dir f = do
  baseDir <- runIO getCurrentDirectory
  testDirs <- runIO $ listDirectory (baseDir ++ dir)
  
  describe desc $ do
    mapM_ (f baseDir) testDirs
    
  runIO $ setCurrentDirectory baseDir
