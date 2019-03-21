module CompilerSpec (spec) where

import Compiler
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import System.FilePath
import Control.Exception

validTestDir curDir = curDir ++ "/test/Programs/ValidTests/"
invalidTestDir curDir = curDir ++ "/test/Programs/InvalidTests/"

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
  curDir <- runIO $ getCurrentDirectory
  
  dirs <- runIO $ listDirectory (validTestDir curDir)
  describe "Valid Tests" $ do
    mapM_ (\dir -> testDir True dir curDir) dirs
  runIO $ setCurrentDirectory curDir

  dirs <- runIO $ listDirectory (invalidTestDir curDir)
  describe "Invalid Tests" $ do
    mapM_ (\dir -> testDir False dir curDir) dirs
  runIO $ setCurrentDirectory curDir  
  
testDir :: Bool -> String -> String -> Spec
testDir b dir curDir = parallel $ do
  sourceFiles <- runIO $ listDirectory ((validOrInvalidDir b) ++ dir)
  let source = getSource sourceFiles
  validOrInvalid source
  where
    validOrInvalid source
      | b = testOne ((validTestDir curDir) ++ dir ++ "/" ++ source) source
      | otherwise = testOneInvalid ((invalidTestDir curDir) ++ dir ++ "/" ++ source) source
    validOrInvalidDir b
      | b = validTestDir curDir
      | otherwise = invalidTestDir curDir
        
getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
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


testOneInvalid :: String -> String -> Spec    
testOneInvalid test filename = do
  compiles <- runIO $ catch (compile test)
                         (\e -> do let err = show (e :: SomeException)
                                   return  $ (False, show e)
                         )
  case compiles of
    (False, err) -> 
      it ("Testing " ++ filename) $ do
        (assertEqual "" 1 1)
        -- assertFailure ("The compiler terminated with errors\n" ++ err)
        return ()
    (True, _)  ->
      it ("Testing " ++ filename) $ do
        assertFailure ("It was expected an error but it's ok")
        return ()

runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  runIO $ putStrLn $ filename ++ " | " ++ testFile
  -- multipleOps.fst | /home/balmeida/workspaces/ContextFreeSession/test/Programs/ValidTests/multipleOps/multipleOps.fst
  runIO $ setCurrentDirectory $ takeDirectory testFile
  (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc"
                                         ["-dynamic", "-XBangPatterns", (replaceExtensions filename "hs")] ""  
  
  if (exitcode == ExitSuccess) then
    do     
      (exitcode1, output1, errors1) <- runIO $ readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
      if (exitcode1 /= ExitSuccess) then
         it ("Testing " ++ filename) $ do
           assertFailure errors1
           return ()
      else do  
         cont <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")
         it ("Testing " ++ filename) $ do
           (filter (/= '\n') output1) `shouldBe` (filter (/= '\n') cont)
  else
    it ("Testing " ++ filename) $ do
      assertFailure errors
      return ()

  

