module Compiler (compileFile) where

import           CodeGen.CodeGen (genProgram)
import           Control.Monad.State
import qualified Data.Set as Set
import           Parse.Parser (parseProgram)
import           Syntax.Expressions (ExpEnv)
import           Syntax.Schemes (TypeEnv, VarEnv)
import           System.Exit
import           System.Process
import           System.IO (stdout)
import           System.Directory
import           System.FilePath
import           Utils.FreestState
import           Utils.PreludeLoader (prelude)
import           Validation.Rename (renameState)
import           Validation.TypeChecking (typeCheck)
import           CodeGen.Annotation
-- import Debug.Trace
-- import Utils.PreludeLoader (isBuiltin)
-- import qualified Data.Map as Map
-- import Syntax.Expressions -- test
-- import Syntax.Show -- test

compileFile :: FilePath -> IO ()
compileFile args 
  | "fst" `isExtensionOf` args = do
      s1 <- parseProgram args prelude
      let s2 = execState renameState s1
      let s3 = execState typeCheck s2
      if hasErrors s3
      then
        die $ getErrors s3
      else
        genCode (varEnv s3) (expEnv s3) (typeEnv s3) args
  | otherwise = die $ "Error: File extension not recognized, provide a .fst file: " ++ args

-- CODE GEN
genCode :: VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO ()
genCode venv eenv tenv path = do
  genProgram venv eenv tenv path
  compileAndRun path

compileAndRun :: String -> IO ()
compileAndRun filepath = do
  let (path, filename) = splitFileName filepath
  changeDir path
  (exitcode, _, errors) <- readProcessWithExitCode "ghc" [targetFileName filename] ""
  checkGhcOut exitcode errors    
  -- (exitcode1, output1, errors1) <- readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
  (_, _, _, handle) <- createProcess (proc "./crisscross" []){ std_out = UseHandle stdout }  
  exitcode1 <- waitForProcess handle
  checkGhcOut exitcode1 ""
--  putStr output1
  exitSuccess

changeDir :: String -> IO ()
changeDir d
  | null d    = return ()
  | otherwise = setCurrentDirectory d

targetFileName :: String -> String
targetFileName file = replaceExtensions file "hs"

checkGhcOut :: ExitCode -> String -> IO ()
checkGhcOut ExitSuccess _ = return ()
checkGhcOut _ errors = putStrLn errors >> exitFailure
