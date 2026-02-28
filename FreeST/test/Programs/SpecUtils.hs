{-# LANGUAGE TupleSections #-}
module SpecUtils where

import Control.Monad (forM_)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
-- import System.Exit (ExitCode (ExitSuccess))
import System.FilePath -- (takeExtension)
import Test.Hspec -- (Spec, runIO, describe, parallel)
import           Control.Monad.Extra

import Test.Hspec
import Util.State (RunOpts)
import Options.Applicative
import Util.CmdLine

getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | ".fst" `isExtensionOf` x = x
  | otherwise = getSource xs

specTestValid :: String -> FilePath -> (FilePath -> (FilePath, String, [String]) -> Expectation) -> Spec
specTestValid desc dir f = do
  baseDir <- runIO getCurrentDirectory
  testDirs <- runIO $ directoryContents (baseDir ++ dir)
  describe desc $
    forM_ testDirs $
      \group -> do
        describe group $ do
          test <- runIO $ directoryContents (baseDir ++ dir ++ group)
          forM_ test $
            \testDir -> let d = baseDir ++ dir ++ group ++ "/" ++ testDir in
            before (beforeHandle d) $
            it (last (splitDirectories testDir) -<.> "fst") $ f d
  where
    beforeHandle :: FilePath -> IO (FilePath, String, [String])
    beforeHandle d = do
      let ds = splitDirectories d -- (baseDir ++ baseTestDir ++ testingDir)
      let progFP = joinPath $ ds ++ [last ds -<.> ".fst"]
      let expFP = progFP -<.> "expected"
      let optsFP = progFP -<.> "opts"
      whenM (not <$> doesFileExist progFP) (error $ "File " ++ progFP ++ " does not exist.")
      whenM (not <$> doesFileExist expFP) (error $ "File " ++ expFP ++ " does not exist.")
      opts <- ifM (doesFileExist optsFP) (words <$> readFile optsFP) (pure [])
      (progFP,, opts) <$> readFile expFP

specTestInvalid :: String -> String -> (String -> String -> Spec) -> Spec
specTestInvalid desc dir f = do
  baseDir <- runIO getCurrentDirectory
  testDirs <- runIO $ directoryContents (baseDir ++ dir)
  parallel $
    describe desc $
      forM_ testDirs $
        \group -> do
          describe group $ do
             test <- runIO $ directoryContents (baseDir ++ dir ++ group)
             forM_ test $
               \testingDir ->
                  f baseDir (group ++ "/" ++ testingDir)

directoryContents :: FilePath -> IO [FilePath]
directoryContents dir =
  filter (('.' /=) . head) <$> listDirectory dir

safeRead :: FilePath -> IO (Maybe String)
safeRead f =
  ifM (doesFileExist f) (fmap Just (readFile f)) (pure Nothing)

parseOpts :: FilePath -> [String] -> RunOpts
parseOpts file opts = case execParserPure (prefs mempty) (info runOptsParser mempty) (file : opts) of
  Success parsedOpts -> parsedOpts
  _                  -> error $ "Error parsing command line options: " ++ unwords opts