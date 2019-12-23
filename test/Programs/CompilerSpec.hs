{-# LANGUAGE ScopedTypeVariables #-}
module CompilerSpec (spec) where

import Compiler
import System.Directory
import System.Process
import System.Exit
import Test.Hspec
import Test.HUnit (assertFailure, assertEqual)
import System.FilePath
import Control.Exception
import GHC.IO.Handle   -- yes, it's GHC-specific
import System.IO
import System.Posix.Redirect
import Data.Char (chr)
import Data.ByteString (ByteString, unpack)

validTestDir curDir = curDir ++ "/test/Programs/ValidTests/"
invalidTestDir curDir = curDir ++ "/test/Programs/InvalidTests/"

main :: IO ()
main = do
  hDuplicateTo stderr stdout
  hspec $ spec

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
      | b         = testOne ((validTestDir curDir) ++ dir ++ "/" ++ source) source
      | otherwise = testOneInvalid ((invalidTestDir curDir) ++ dir ++ "/" ++ source) source
    validOrInvalidDir b
      | b         = validTestDir curDir
      | otherwise = invalidTestDir curDir
        
getSource :: [String] -> String
getSource [] = ""
getSource (x:xs)
  | takeExtension x == ".fst" = x
  | otherwise = getSource xs
  
-- exitProgram :: ExitCode -> IO (Bool, String)
-- exitProgram ExitSuccess = return (True, "")
-- exitProgram e = return (False, show e)

exitProgram :: ExitCode -> IO Bool
exitProgram ExitSuccess = return True
exitProgram e = return False


testOne :: String -> String -> Spec    
testOne test filename = do
  (_,(err, b)) <- runIO $ redirectStdout $ redirectStderr $
    catches (compileFile test >> return True)
                  [Handler (\(e :: ExitCode) -> exitProgram e),
                   Handler (\(e :: SomeException) -> return False)]                  

  if b then
    runAndCheckResult test filename   
  else
    it ("Testing " ++ filename) $ do
      assertFailure (bsToStr err) 
      return ()    

testOneInvalid :: String -> String -> Spec    
testOneInvalid test filename = do
-- redirectStdout :: IO a -> IO (ByteString, a)
  (_ , (b, err)) <- runIO $ redirectStderr $
                        catch (compileFile test >> return (True,""))
                         (\e -> return  $ (False, show (e :: SomeException)))
  if b then
    it ("Testing " ++ filename) $ do
      assertFailure ("It was expected an error but none was thrown")
      return ()
  else
   it ("Testing " ++ filename) $ do
      (assertEqual "OK. Passed!" 1 1)

-- Review - code gen
runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  runIO $ setCurrentDirectory $ takeDirectory testFile
  (exitcode, _, errors) <- runIO $
                                 readProcessWithExitCode "ghc"
                                   ["-dynamic", "-XBangPatterns", (replaceExtensions filename "hs")] ""  
  
  if (exitcode == ExitSuccess) then
    do     
      (exitcode1, output1, errors1) <- runIO $ readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
      if (exitcode1 /= ExitSuccess) then
         it ("Testing " ++ filename) $ do
           assertFailure errors1
           return ()
      else do  
         exp <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")
         it ("Testing " ++ filename) $ do
           (filter (/= '\n') output1) `shouldBe` (filter (/= '\n') exp)
  else
    it ("Testing " ++ filename) $ do
      assertFailure errors
      return ()


bsToStr :: ByteString -> String
bsToStr = map (chr . fromEnum) . unpack
