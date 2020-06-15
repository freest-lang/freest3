module Compiler (compileFile) where

import CodeGen.CodeGen (genProgram)
import Control.Monad.State
import Parse.Parser (parseProgram)
import Syntax.Expressions (ExpEnv)
import Syntax.Schemes (TypeEnv, VarEnv)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (stdout)
import System.Process
import Utils.FreestState
import Utils.PreludeLoader (prelude)
import Validation.Rename (renameState)
import Validation.TypeChecking (typeCheck)
import Validation.BuildTypes
import Interpreter.Eval
import Interpreter.Value
import Interpreter.Builtin

import qualified Data.Map.Strict as Map
import Syntax.Base 


compileFile :: FilePath -> IO ()
compileFile args
  | "fst" `isExtensionOf` args = do
      -- Parsing
      s1 <- parseProgram args prelude
      when (hasErrors s1) (die $ getErrors s1)
      -- Renaming the state
      let s2 = execState renameState s1
      -- Solving type declarations and dualofs
      let s3 = execState solveTypeDecls s2
      when (hasErrors s3) (die $ getErrors s3)
      -- TypeChecking
      let s4 = execState typeCheck s3
      when (hasErrors s4) (die $ getErrors s4)
      -- Code Generation
      -- genCode (varEnv s4) (expEnv s4) (typeEnv s4) args
      res <- eval initialCtx (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
      case res of
        IOValue io -> io >>= putStrLn . show
        _          -> putStrLn $ show res
      return ()
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
  (_, _, _, handle) <- createProcess (proc ("./" ++ dropExtension filename) []){ std_out = UseHandle stdout }  
  exitcode1 <- waitForProcess handle
  checkGhcOut exitcode1 ""
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
