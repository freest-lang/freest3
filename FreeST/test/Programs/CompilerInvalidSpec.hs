{-# LANGUAGE ScopedTypeVariables, LambdaCase, BlockArguments #-}
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
-- import           Util.FreestState
import           Util.State hiding (void)

import           System.FilePath
import           Data.List
import Control.Monad.Extra (ifM)

baseTestDir :: String
baseTestDir = "/test/Programs/InvalidTests/"

spec :: Spec
spec = specTestInvalid "Invalid Tests" baseTestDir testDir

testDir :: String -> String -> Spec
testDir baseDir invalidTest = do
  let dir = baseDir ++ baseTestDir ++ invalidTest
  sourceFiles <- runIO $ listDirectory dir
  let source = getSource sourceFiles
  testInvalid (dir ++ "/" ++ source) source


testInvalid :: FilePath -> String -> Spec
testInvalid test filename = do
  let optsFP = test -<.> "opts"
  opts <- runIO $ ifM (doesFileExist optsFP) (words <$> readFile optsFP) (pure [])
  b <- runIO $ hSilence [stdout, stderr] $ catches
    do checkAndRun (parseOpts test opts){quietmode = True}
       return (Just errorExpected)
    [ Handler (\(e :: ExitCode) -> return $ exitProgram e)
    , Handler (\(e :: SomeException) -> return $ Just $ "(Internal error) "++show e)
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
