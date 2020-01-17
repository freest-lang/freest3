{-# LANGUAGE ScopedTypeVariables #-}
module CompilerValidSpec (spec) where

import Compiler
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import System.FilePath
import Control.Exception
import System.IO (stdout, stderr)
import System.IO.Silently(hCapture)

baseTestDir :: String -> String
baseTestDir baseDir = baseDir ++ "/test/Programs/ValidTests/"

spec :: Spec
spec = do
  baseDir <- runIO $ getCurrentDirectory 
  testDirs <- runIO $ listDirectory (baseTestDir baseDir)
  
  describe "Valid Tests" $ do
    mapM_ (testValid baseDir) testDirs
    
  runIO $ setCurrentDirectory baseDir

getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
  | otherwise = getSource xs

exitProgram :: ExitCode -> IO Bool
exitProgram ExitSuccess = return True
exitProgram e = return False

testValid :: String -> String -> Spec
testValid baseDir testingDir = do
  sourceFiles <- runIO $ listDirectory (baseTestDir baseDir ++ testingDir)
  let source = getSource sourceFiles

  let curTestDir = (baseTestDir baseDir ++ testingDir ++ "/" ++ source) 
  (b, err) <- runIO $ testOne curTestDir source

  if b then do
    runAndCheckResult curTestDir source
  else  
    it ("Testing " ++ source) $ do
      assertFailure err
      return ()


testOne :: String -> String -> IO (Bool, String)
testOne test filename = do
  (err, b) <- hCapture [stdout, stderr] $ catches (compileFile test >> return True)
                  [Handler (\(e :: ExitCode) -> exitProgram e),
                   Handler (\(e :: SomeException) -> return False)]
  return (b, err)
  
runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  runIO $ setCurrentDirectory $ takeDirectory testFile
  (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc"
    ["-dynamic", "-XBangPatterns", (replaceExtensions filename "hs")] ""  
  
  if (exitcode == ExitSuccess) then do     
      (exitcode1, output1, errors1) <-
         runIO $ readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
      if (exitcode1 /= ExitSuccess) then
         it ("Testing " ++ filename) $ assertFailure errors1 >> return ()
      else do  
         exp <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")
         it ("Testing " ++ filename) $ 
           (filter (/= '\n') output1) `shouldBe` (filter (/= '\n') exp)
  else
    it ("Testing " ++ filename) $ assertFailure errors >> return ()
