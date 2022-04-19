module SpecUtils where

import Control.Monad (forM_)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
-- import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeExtension)
import Test.Hspec (Spec, runIO, describe, parallel)
import           Control.Monad.Extra

import Test.Hspec

getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
  | otherwise = getSource xs

specTest :: String -> String -> (String -> String -> Spec) -> Spec
specTest desc dir f = do
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
  
    -- describe desc $
    --   forM_ testDirs $ f baseDir


directoryContents :: FilePath -> IO [FilePath]
directoryContents dir =
  filter (('.' /=) . head) <$> listDirectory dir


safeRead :: FilePath -> IO (Maybe String)
safeRead f =
  ifM (doesFileExist f) (fmap Just (readFile f)) (pure Nothing)
