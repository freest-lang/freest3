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
  

exitProgram :: ExitCode -> IO (Bool, String)
exitProgram ExitSuccess = return (True, "")
exitProgram e = return (False, show e) 

testOne :: String -> String -> Spec    
testOne test filename = do
  (b, err) <- runIO $
    catches (compileFile test >> return (True, ""))
                  [Handler (\(e :: ExitCode) -> exitProgram e),
                   Handler (\(e :: SomeException) -> return (False, show e))]                  

 -- Removed code gen
  if b then
    -- runAndCheckResult test filename   
    it ("Testing " ++ filename) $ do
      assertEqual "Passed... without code gen" 1 1
      return ()
  else
    it ("Testing " ++ filename) $ do
      assertFailure ("The compiler terminated with errors (check above)\n")
      -- TODO: errors should appear here
      return ()    

testOneInvalid :: String -> String -> Spec    
testOneInvalid test filename = do
  (b, err) <- runIO $ catch (compileFile test >> return (True,""))
                        (\e -> return  $ (False, show (e :: SomeException)))
  if b then
    it ("Testing " ++ filename) $ do
      assertFailure ("It was expected an error but it's ok")
      return ()
  else
   it ("Testing " ++ filename) $ do
        (assertEqual "OK. Passed!" 1 1)

-- Review - code gen
-- runAndCheckResult :: String -> String -> Spec
-- runAndCheckResult testFile filename = do
--   runIO $ setCurrentDirectory $ takeDirectory testFile
--   (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc"
--                                          ["-dynamic", "-XBangPatterns", (replaceExtensions filename "hs")] ""  
  
--   if (exitcode == ExitSuccess) then
--     do     
--       (exitcode1, output1, errors1) <- runIO $ readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
--       if (exitcode1 /= ExitSuccess) then
--          it ("Testing " ++ filename) $ do
--            assertFailure errors1
--            return ()
--       else do  
--          cont <- runIO $ readFile ((takeWhile (/= '.') testFile) ++ ".expected")
--          it ("Testing " ++ filename) $ do
--            (filter (/= '\n') output1) `shouldBe` (filter (/= '\n') cont)
--   else
--     it ("Testing " ++ filename) $ do
--       assertFailure errors
--       return ()

  

