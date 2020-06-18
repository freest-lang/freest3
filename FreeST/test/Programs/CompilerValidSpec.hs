{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module CompilerValidSpec (spec) where

import FreeST
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import System.FilePath
import Control.Exception
import System.IO (stdout, stderr)
import System.IO.Silently(hCapture)
import System.Timeout
import Control.Monad
import Data.Maybe


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

testValid :: String -> String -> Spec
testValid baseDir testingDir = do
  sourceFiles <- runIO $ listDirectory (baseTestDir baseDir ++ testingDir)
  let source = getSource sourceFiles
  let curTestDir = (baseTestDir baseDir ++ testingDir ++ "/" ++ source)

  (res, m) <- runIO (testOne curTestDir)
  testProgram m res source curTestDir

  where
    testProgram :: Bool -> String -> FilePath -> FilePath -> Spec
    testProgram m res source curTestDir
      | m         = testWithExpected curTestDir source res
      | otherwise = it ("Testing " ++ source) $ failTest res
      
    failTest :: String -> IO ()
    failTest []  = void $ assertFailure "Timeout (loop)"
    failTest err = void $ assertFailure err
 
testOne :: FilePath -> IO (String, Bool)
testOne file = do
  hCapture [stdout, stderr] $ catches test
                 [Handler (\(e :: ExitCode)      -> exitProgram e),
                  Handler (\(e :: SomeException) -> return False)]

  where
    test :: IO Bool 
    test =
      liftM isJust (timeout 5000000 (compileFile file))         

    exitProgram :: ExitCode -> IO Bool
    exitProgram ExitSuccess = return True
    exitProgram _           = return False

testWithExpected :: FilePath -> FilePath -> String -> Spec
testWithExpected curTestDir source res = do
  let expectedFile = (takeWhile (/= '.') curTestDir) ++ ".expected"
  exp <- runIO $ safeRead expectedFile
  case exp of
    Just s -> do
      it ("Testing " ++ source) $
        (filter (/= '\n') res) `shouldBe` (filter (/= '\n') s)
    Nothing ->
      it ("Testing " ++ source) $
        void $ assertFailure $ "File " ++ expectedFile ++ " not found"
  where
    safeRead :: FilePath -> IO (Maybe String)
    safeRead f = do
      b <- doesFileExist f
      if b then liftM Just (readFile f) else return Nothing
    
