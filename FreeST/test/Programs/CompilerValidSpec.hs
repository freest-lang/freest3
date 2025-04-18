{-# LANGUAGE ScopedTypeVariables, LambdaCase, BangPatterns #-}
module CompilerValidSpec
  ( spec
  )
where

import Control.Exception
import Control.Monad ( void, unless )
import Control.Monad.Extra
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import FreeST ( checkAndRun )
import GHC.IO.Handle
import SpecUtils
import System.Directory
import System.Exit
import System.FilePath -- ((</>))
import           System.IO                      ( stdout
                                                , stderr
                                                )
import System.IO.Silently -- ( hCapture )
import System.Timeout
import Test.HUnit ( assertFailure )
import Test.Hspec
-- import Util.FreestState
import           Util.State hiding (void)

data TestResult = Timeout | Passed | Failed

baseTestDir :: String
baseTestDir = "/test/Programs/ValidTests/"

spec :: Spec
spec = specTest' "Valid Tests" baseTestDir validTest
-- spec = specTest "Valid Tests" baseTestDir testValid

validTest :: FilePath -> (FilePath, String) -> Expectation
validTest dir (testFile, exp) = 
  if "<pending>" `isPrefixOf` exp
    then pendingMessage exp
    else do
     (out, res) <- testOne testFile
     case res of
      Timeout -> doExpectationsMatch "<timeout>" exp
      Failed  -> void $ assertFailure out
      Passed  -> doExpectationsMatch out exp

pendingMessage :: String -> Expectation
pendingMessage =  pendingWith . intercalate "\n\t" . tail . lines

doExpectationsMatch :: String -> String -> Expectation
doExpectationsMatch out exp = out `shouldSatisfy` (`elem` expectedOutputs)
  where
    expectedOutputs = map (read . (\line -> "\"" ++ line ++ "\"")) . lines $ exp

testOne :: FilePath -> IO (String, TestResult)
testOne file = hCapture [stdout, stderr] $
   catches runTest
    [ Handler (\(e :: ExitCode) -> exitProgram e)
    , Handler (\(_ :: SomeException) -> pure Failed)
    ]
 where
  exitProgram :: ExitCode -> IO TestResult
  exitProgram ExitSuccess = pure Passed
  exitProgram _           = pure Failed

  runTest :: IO TestResult
  runTest =
    timeout timeInMicro (checkAndRun defaultOpts { runFilePath = file, quietmode = True })
      >>= \case
            Just _  -> pure Passed
            Nothing -> pure Timeout

-- n microseconds (1/10^6 seconds).
timeInMicro :: Int
timeInMicro = 3 * 1000000
