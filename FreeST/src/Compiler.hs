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
import           Utils.PreludeLoader (prelude, userDefined) -- debug: userDefined
import           Validation.Rename (renameState)
import           Validation.TypeChecking (typeCheck)
import           CodeGen.Annotation
-- import Debug.Trace
-- import Utils.PreludeLoader (isBuiltin)
-- import qualified Data.Map as Map
-- import Syntax.Expressions -- test
-- import Syntax.Show -- test
import Validation.BuildTypes 

-- TODO: one more if here; if a parse error occured we should not continue
compileFile :: FilePath -> IO ()
compileFile args 
  | "fst" `isExtensionOf` args = do
      s1 <- parseProgram args prelude
      let s2 = execState renameState s1
      let s3 = execState solveTypeDecls s2
      when (hasErrors s3) (die $ getErrors s3)     
      let s4 = execState typeCheck s3
      when (hasErrors s4) (die $ getErrors s4)
      genCode (varEnv s4) (expEnv s4) (typeEnv s4) args
      
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
  (_, _, _, handle) <- createProcess (proc ("./" ++ dropExtension filename) []){ std_out = UseHandle stdout }  
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
