{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module CompilerInvalidSpec
  ( spec
  )
where

import           Control.Exception
import           Control.Monad                  ( void )
import           FreeST                         ( checkAndRun )
import           SpecUtils
import           System.Directory
import           System.Exit
import           System.IO                      ( stdout
                                                , stderr
                                                )
import           System.IO.Silently             ( hSilence )
import           Test.HUnit                     ( assertFailure
                                                , assertEqual
                                                )
import           Test.Hspec
import           Util.FreestState

import           System.FilePath
import           Data.List

baseTestDir :: String
baseTestDir = "/test/Programs/InvalidTests/"

spec :: Spec
spec = specTest "Invalid Tests" baseTestDir testDir

testDir :: String -> String -> Spec
testDir baseDir invalidTest = do
  let dir = baseDir ++ baseTestDir ++ invalidTest
  sourceFiles <- runIO $ listDirectory dir
  let source = getSource sourceFiles
  testInvalid (dir ++ "/" ++ source) source


testInvalid :: String -> String -> Spec
testInvalid test filename = do
  b <- runIO $ hSilence [stdout, stderr] $ catches
    (  checkAndRun defaultOpts { runFilePath = test, quietmode = True }
    >> return (Just errorExpected)
    )
    [ Handler (\(e :: ExitCode) -> return $ exitProgram e)
    , Handler (\(_ :: SomeException) -> return $ Just "Internal error thrown")
    ]
  assert b
 where
  assert b = do
    let expected = test -<.> "expected"
    runIO (safeRead expected) >>= \case
      Just s
        | "<pending>" `isPrefixOf` s  ->
            it ("Testing " ++ takeBaseName expected) $
              pendingWith $ intercalate "\n\t" $ tail $ lines s
        | otherwise                   -> assert' b
      Nothing  ->  assert' b
    
  assert' (Just err) = it ("Testing " ++ filename) $ void $ assertFailure err
  assert' _ = it ("Testing " ++ filename) $ assertEqual "OK. Passed!" 1 1

  
exitProgram :: ExitCode -> Maybe String
exitProgram ExitSuccess = Just errorExpected
exitProgram _           = Nothing

errorExpected :: String
errorExpected = "An error was expected but none was thrown"
