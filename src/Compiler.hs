module Compiler (compileFile) where

import           CodeGen.CodeGen (genProgram)
import           Control.Monad.State
import qualified Data.Set as Set
import           Parse.Parser (parseProgram)
import           Syntax.Expressions (ExpEnv)
import           Syntax.Schemes (TypeEnv, VarEnv)
import           System.Exit
import           System.Process
import           System.Directory
import           System.FilePath
import           Utils.FreestState
import           Utils.PreludeLoader (prelude)
import           Validation.Rename (renameState)
import           Validation.TypeChecking (typeCheck)
import CodeGen.Annotation
import Debug.Trace
import Utils.PreludeLoader (isBuiltin)
import qualified Data.Map as Map
import Syntax.Expressions -- test
import Syntax.Show -- test

compileFile :: FilePath -> IO ()
compileFile args 
  | "fst" `isExtensionOf` args = do
      s1 <- parseProgram args prelude
      let s2 = execState renameState s1
      let s3 = execState typeCheck s2
      genCode (errors s3) (varEnv s3) (expEnv s3) (typeEnv s3) args
  | otherwise = die $ "Error: File extension not recognized, provide a .fst file: " ++ args

-- CODE GEN
genCode :: Errors -> VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO ()
genCode err venv eenv tenv path
  | Set.null err  = do
      -- traceM "Starting Code Gen"
      -- let t = translateEnv eenv tenv venv
      -- let (t1,t2) = topExps eenv tenv venv
      -- putStrLn $ show (Map.filterWithKey (\k _ -> not (isBuiltin k)) t1)

      -- putStrLn $ "\n" ++ showAST t2 ++ ">" 
      
      genProgram venv eenv tenv path
      compileAndRun path
  | otherwise = die $ getErrors err

-- showAST :: AST -> String
-- showAST = Map.foldlWithKey (\acc e t -> acc ++ "(" ++ show (position e) ++ " ~ " ++ show e ++ ", " ++ show t ++ ") " ) "< "

compileAndRun :: String -> IO ()
compileAndRun filepath = do
  let (path, filename) = splitFileName filepath
  changeDir path
  (exitcode, _, errors) <- readProcessWithExitCode "ghc" [targetFileName filename] ""
  checkGhcOut exitcode errors
    
  (exitcode1, output1, errors1) <- readProcessWithExitCode ("./" ++ dropExtension filename) [] ""
  checkGhcOut exitcode1 errors1
  putStr output1
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
