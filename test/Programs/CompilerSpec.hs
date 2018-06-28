module CompilerSpec (spec) where

import           Compiler
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           System.Process
import           System.Exit
import           Test.Hspec
import           Test.HUnit (assertFailure)

import Control.Exception

-- baseDir = "/test/Programs/ValidTests/"
validTestDir curDir = curDir ++ "/test/Programs/ValidTests/"
invalidTestDir curDir = curDir ++ "/test/Programs/InvalidTests/"

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
   -- TESTING
  curDir <- runIO $ getCurrentDirectory
  dirs <- runIO $ listDirectory (validTestDir curDir)

  describe "Valid Tests" $ do
    mapM_ (\dir -> testDir dir curDir) dirs

  -- dirs <- runIO $ listDirectory (invalidTestDir curDir)
  -- describe "Invalid Tests" $ do
  --   mapM_ (\dir -> testDir dir curDir) dirs

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
  | ".hs" `isInfixOf` x && x /= "cfst.hs" && x /= "CFSTUtils.hs" = x
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

-- t6 = catch (evaluate lin)
--      (\e -> do let err = show (e :: SomeException)
--                return $ (False, ("Warning: Couldn't open " ++ ": " )))  

runAndCheckResult :: String -> String -> Spec
runAndCheckResult testFile filename = do
  let path = reverse $ dropWhile (/= '/') (reverse testFile)
  runIO $ setCurrentDirectory path
 
  (exitcode, output, errors) <- runIO $ readProcessWithExitCode "ghc" ["-dynamic", "-XBangPatterns", "cfst.hs"] ""  
  -- (exitcode, output, errors) - runIO $ readProcessWithExitCode "ghc" ["cfst.hs", "-fno-code", "-O0"] ""

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

  

