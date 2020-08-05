{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module CompilerValidSpec (spec) where

import Control.Exception
import Control.Monad
import Data.Maybe
import FreeST
import SpecUtils
import System.Directory
import System.Exit
import System.IO (stdout, stderr)
import System.IO.Silently (hCapture)
import System.Timeout
import Test.HUnit (assertFailure)
import Test.Hspec

import System.FilePath -- ((</>))

baseTestDir :: String
baseTestDir = "/test/Programs/ValidTests/"

spec :: Spec
spec = specTest "Valid Tests" baseTestDir testValid 

testValid :: String -> String -> Spec
testValid baseDir testingDir = do
  let dir = baseDir ++ baseTestDir ++ testingDir
  sourceFiles <- runIO $ listDirectory dir
  file <- runIO $ liftM getSource (listDirectory dir)
  let f = dir </> file
  testResult <- runIO $ testOne f
  checkResult testResult f

testOne :: FilePath -> IO (String, Bool)
testOne file = do
  hCapture [stdout, stderr] $
    catches (liftM isJust (timeout 5000000 (compileFile file)))
      [Handler (\(e :: ExitCode)      -> exitProgram e),
       Handler (\(_ :: SomeException) -> return False)]
  where
    exitProgram :: ExitCode -> IO Bool
    exitProgram ExitSuccess = return True
    exitProgram _           = return False

checkResult :: (String, Bool) -> FilePath -> Spec
checkResult (res, hasErrors) file
  | hasErrors = checkAgainstExpected file res
  | otherwise = it ("Testing " ++ takeFileName file) $ failTest res
  where
    failTest :: String -> IO ()
    failTest []  = void $ assertFailure "Timeout (loop)"
    failTest err = void $ assertFailure err
   
checkAgainstExpected :: FilePath -> String -> Spec
checkAgainstExpected file res = do
  let expFile = file -<.> "expected"
  runIO (safeRead expFile) >>= \case
    Just s -> 
      it ("Testing " ++ takeFileName file) $
        (filter (/= '\n') res) `shouldBe` (filter (/= '\n') s)
    Nothing ->
      it ("Testing " ++ takeFileName file) $
        void $ assertFailure $ "File " ++ expFile ++ " not found"
        
  where
    safeRead :: FilePath -> IO (Maybe String)
    safeRead f = do
      b <- doesFileExist f
      if b then liftM Just (readFile f) else return Nothing
        
