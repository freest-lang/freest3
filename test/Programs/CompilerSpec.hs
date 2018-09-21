module CompilerSpec (spec) where

import Compiler
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure)
import System.FilePath
import Control.Exception

validTestDir curDir = curDir ++ "/test/Programs/ValidTests/"
-- invalidTestDir curDir = curDir ++ "/test/Programs/InvalidTests/"

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
  curDir <- runIO $ getCurrentDirectory
  dirs <- runIO $ listDirectory (validTestDir curDir)

  describe "Valid Tests" $ do
    mapM_ (\dir -> testDir dir curDir) dirs
    
  runIO $ setCurrentDirectory curDir

-- TODO: test invalid with expect failure
testDir :: String -> String -> Spec
testDir dir curDir = parallel $ do
  sourceFiles <- runIO $ listDirectory ((validTestDir curDir) ++ dir)
  let source = getSource sourceFiles
  testOne ((validTestDir curDir) ++ dir ++ "/" ++ source) source

  
getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".cfs" = x
  | otherwise = getSource xs

testOne :: String -> String -> Spec    
testOne test filename = do
  compiles <- runIO $ catch (compile test)
                         (\e -> do let err = show (e :: SomeException)
                                   return  $ (False, show e)
                         )
  case compiles of
    (False, err) -> 
      it ("Testing " ++ filename) $ do
        assertFailure ("The compiler terminated with errors\n" ++ err)
        return ()
    (True, _)  -> runAndCheckResult test filename

runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  
  runIO $ setCurrentDirectory $ takeDirectory testFile
  (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc" ["-dynamic", "-XBangPatterns", "cfst.hs"] ""  
 
  if (exitcode == ExitSuccess) then
    do     
      (exitcode1, output1, errors1) <- runIO $ readProcessWithExitCode "./cfst" [] ""
      cont <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")

      it ("Testing " ++ filename) $ do
        (filter (/= '\n') output1) `shouldBe` (filter (/= '\n') cont)
  else
    it ("Testing " ++ filename) $ do
      assertFailure errors
      return ()

  

