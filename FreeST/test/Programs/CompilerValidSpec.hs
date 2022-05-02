{-# LANGUAGE ScopedTypeVariables, LambdaCase, BangPatterns #-}
module CompilerValidSpec
  ( spec
  )
where

import           Control.Exception
import           Control.Monad                  ( void, unless )
import           Control.Monad.Extra
import           FreeST                         ( checkAndRun )
import           SpecUtils
import           System.Directory
import           System.Exit
import           System.IO                      ( stdout
                                                , stderr
                                                )
import           System.IO.Silently            -- ( hCapture )
import           System.Timeout
import           Test.HUnit                     ( assertFailure )
import           Test.Hspec
import           Util.FreestState
import           System.FilePath -- ((</>))
import Data.List
import GHC.IO.Handle
import System.IO
import System.Directory

import Debug.Trace

data TestResult = Timeout | Passed | Failed

baseTestDir :: String
baseTestDir = "/test/Programs/ValidTests/"

spec :: Spec
spec = specTest' "Valid Tests" baseTestDir validTest
-- spec = specTest "Valid Tests" baseTestDir testValid

validTest :: FilePath -> (FilePath, String) -> Expectation
validTest dir (testFile, exp) = do
  (out, res) <- testOne testFile
--  let out = exp

  case res of
    Timeout -> doExpectationsMatch exp "<divergent>"
    Failed  -> void $ assertFailure out
    Passed
      | "<pending>" `isPrefixOf` exp -> pendingWith $ intercalate "\n\t" $ tail $ lines exp
      | otherwise -> doExpectationsMatch exp out


doExpectationsMatch :: String -> String -> Expectation
doExpectationsMatch out exp =
  filter (/= '\n') out `shouldBe` filter (/= '\n') exp

-- testValid :: String -> String -> Spec
-- testValid baseDir testingDir = do
--   let dir = baseDir ++ baseTestDir ++ testingDir
--   file <- runIO $ fmap getSource (listDirectory dir)
--   let f = dir </> file
--   let expectedFile = f -<.> "expected"
--  -- unless (pending f) $ do
--   runIO (readExpected expectedFile) >>= \case
--     Left s -> s
--     Right s -> checkResult s expectedFile =<< runIO (testOne f)
    
--  where
--    readExpected :: FilePath -> IO (Either Spec String)
--    readExpected expectedFile = do
--      safeRead expectedFile >>= \case
--        Just s
--          | "<pending>" `isPrefixOf` s  -> pure $ Left $
--               it ("Testing " ++ takeBaseName expectedFile) $
--                 pendingWith $ intercalate "\n\t" $ tail $ lines s
--          | otherwise -> pure $ Right s
--        Nothing ->
--          pure $ Left $
--            it ("Testing " ++ takeBaseName expectedFile) $
--              void $ assertFailure $ "File " ++ expectedFile ++ " not found"

  
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

-- checkResult :: String -> String -> (String, TestResult) -> Spec
-- checkResult contents file (_, Timeout) = checkAgainstExpected file contents "<divergent>"
-- checkResult contents file (res, Passed) = checkAgainstExpected file contents res
-- checkResult _ file (res, Failed) = it ("Testing " ++ takeBaseName file) $ void $ assertFailure res


-- checkAgainstExpected :: FilePath -> String -> String -> Spec
-- checkAgainstExpected expectedFile expectedContents result =
--   it ("Testing " ++ takeBaseName expectedFile) $
--      filter (/= '\n') result `shouldBe` filter (/= '\n') expectedContents


-- checkResult :: (String, TestResult) -> String -> Spec
-- checkResult (res, Passed) file = checkAgainstExpected file res
-- checkResult (res, Failed) file = it ("Testing " ++ takeFileName file) $ void $ assertFailure res
-- checkResult (_, Timeout) file = checkAgainstExpected file "<divergent>"

-- checkAgainstExpected :: FilePath -> String -> Spec
-- checkAgainstExpected file res = do
--   let expFile = file -<.> "expected"
--   runIO (safeRead expFile) >>= \case
--     Just s ->
--       it ("Testing " ++ takeFileName file) $
--         filter (/= '\n') res `shouldBe` filter (/= '\n') s
--     Nothing ->
--       it ("Testing " ++ takeFileName file) $
--         void $ assertFailure $ "File " ++ expFile ++ " not found"
--  where
--   safeRead :: FilePath -> IO (Maybe String)
--   safeRead f = do
--     b <- doesFileExist f
--     if b then fmap Just (readFile f) else return Nothing
