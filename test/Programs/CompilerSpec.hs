module Programs.CompilerSpec (spec) where

import           Compiler
import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           System.Process
import           System.Exit
import           Test.Hspec
import           Test.HUnit


-- baseDir = "/test/Programs/ValidTests/"
validTestDir curDir = curDir ++ "/test/Programs/ValidTests/"
invalidTestDir curDir = curDir ++ "/test/Programs/InvalidTests/"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   -- TESTING
  curDir <- runIO $ getCurrentDirectory
  dirs <- runIO $ listDirectory (validTestDir curDir)

  describe "Valid Tests" $ do
    mapM_ (\dir -> testDir dir curDir) dirs

  dirs <- runIO $ listDirectory (invalidTestDir curDir)
  describe "Invalid Tests" $ do
    mapM_ (\dir -> testDir dir curDir) dirs

-- TODO: test invalid with expect failure
testDir :: String -> String -> Spec
testDir dir curDir = do
  sourceFiles <- runIO $ listDirectory ((validTestDir curDir) ++ dir)
  let source = getSource sourceFiles
  testOne ((validTestDir curDir) ++ dir ++ "/" ++ source) source
  
getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | ".hs" `isInfixOf` x = x
  | otherwise = getSource xs

testOne :: String -> String -> Spec
testOne test filename = do
  compiles <- runIO $ compile test
  case compiles of
    False -> 
      it ("Testing " ++ filename) $ do
        assertFailure "The compiler terminated with errors"
    True  -> runAndCheckResult test filename



runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc" ["cfst.hs"] ""
  -- putStrLn $ "exitcode: " ++ show exitcode
  -- putStrLn $ "output: " ++ show output
  if (exitcode == ExitSuccess) then
    do
      (exitcode1, output1, errors1) <- runIO $ readProcessWithExitCode "./cfst" [] ""
      cont <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")

      it ("Testing " ++ filename) $ do
        cont `shouldBe` output1
        
  else
     it ("Testing " ++ filename) $ do
      assertFailure errors

  -- runIO $ putStrLn $ "errors: " ++ show errors

  -- putStrLn $ "exitcode1: " ++ show exitcode1
  -- putStrLn $ "output1: " ++ show output1
  -- putStrLn $ "errors1: " ++ show errors1
  

