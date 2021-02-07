{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module CompilerValidSpec (spec) where

import Control.Exception
import Control.Monad (void)
import FreeST (checkAndRun)
import SpecUtils
import System.Directory
import System.Exit
import System.IO (stdout, stderr)
import System.IO.Silently (hCapture)
import System.Timeout
import Test.HUnit (assertFailure)
import Test.Hspec

import System.FilePath -- ((</>))

data TestResult = Timeout | Passed | Failed

baseTestDir :: String
baseTestDir = "/test/Programs/ValidTests/"

spec :: Spec
spec = specTest "Valid Tests" baseTestDir testValid 

testValid :: String -> String -> Spec
testValid baseDir testingDir = do
  let dir = baseDir ++ baseTestDir ++ testingDir
  file <- runIO $ fmap getSource (listDirectory dir)
  let f = dir </> file
  testResult <- runIO $ testOne f
  checkResult testResult f

testOne :: FilePath -> IO (String, TestResult)
testOne file =
 hCapture [stdout, stderr] $
   catches runTest
      [Handler (\(e :: ExitCode)      -> exitProgram e),
       Handler (\(_ :: SomeException) -> pure Failed)]
  where
    exitProgram :: ExitCode -> IO TestResult
    exitProgram ExitSuccess = pure Passed -- return True
    exitProgram _           = pure Failed   -- return False

    runTest :: IO TestResult
    runTest =
      timeout timeInMicro (checkAndRun file) >>= \case
        Just _  -> pure Passed
        Nothing -> pure Timeout

-- n microseconds (1/10^6 seconds).
timeInMicro :: Int
timeInMicro = 2 * 1000000

checkResult :: (String, TestResult) -> FilePath -> Spec
checkResult (res, Passed) file = checkAgainstExpected file res
checkResult (res, Failed) file = 
  it ("Testing " ++ takeFileName file) $
    void $ assertFailure res
checkResult (_, Timeout) file  = checkAgainstExpected file "<divergent>"

checkAgainstExpected :: FilePath -> String -> Spec
checkAgainstExpected file res = do
  let expFile = file -<.> "expected"
  runIO (safeRead expFile) >>= \case
    Just s ->
      it ("Testing " ++ takeFileName file) $
        filter (/= '\n') res `shouldBe` filter (/= '\n') s
    Nothing ->
      it ("Testing " ++ takeFileName file) $
        void $ assertFailure $ "File " ++ expFile ++ " not found"        
  where
    safeRead :: FilePath -> IO (Maybe String)
    safeRead f = do
      b <- doesFileExist f
      if b then fmap Just (readFile f) else return Nothing
        
