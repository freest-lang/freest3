{-# LANGUAGE ScopedTypeVariables #-}
module CompilerInvalidSpec (spec) where

import FreeST
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import System.FilePath
import Control.Exception
import System.IO (stdout, stderr)
import System.IO.Silently (hSilence)

baseTestDir :: String -> String
baseTestDir baseDir = baseDir ++ "/test/Programs/InvalidTests/"

spec :: Spec
spec = do
  baseDir <- runIO $ getCurrentDirectory 
  testDirs <- runIO $ listDirectory (baseTestDir baseDir)
  
  describe "Invalid Tests" $ do
    mapM_ (testDir baseDir) testDirs
    
  runIO $ setCurrentDirectory baseDir

testDir :: String -> String -> Spec
testDir baseDir invalidTest = do
  sourceFiles <- runIO $ listDirectory (baseTestDir baseDir ++ invalidTest)
  let source = getSource sourceFiles
  testInvalid ((baseTestDir baseDir) ++ invalidTest ++ "/" ++ source) source
  
getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
  | otherwise = getSource xs

exitProgram :: ExitCode -> IO Bool
exitProgram ExitSuccess = return True
exitProgram e = return False

testInvalid :: String -> String -> Spec    
testInvalid test filename = do
  b <- runIO $ hSilence [stdout, stderr] $ 
    catches (compileFile test >> return True)
                  [Handler (\(e :: ExitCode) -> exitProgram e),
                   Handler (\(e :: SomeException) -> return False)]

  if b then
    it ("Testing " ++ filename) $
      assertFailure ("It was expected an error but none was thrown") >> return ()
  else
   it ("Testing " ++ filename) $ (assertEqual "OK. Passed!" 1 1)

