module Compiler (compile) where

import Syntax.Schemes (TypeEnv, VarEnv)
import Syntax.Expressions (ExpEnv)
import Parse.Parser (parseProgram)
import CodeGen.CodeGen (genProgram)
import Validation.TypeChecking (typeCheck)
import Utils.PreludeLoader (prelude)
import Utils.FreestState
import Control.Monad.State
import Data.List (intercalate)


compile :: String -> IO (Bool, String)
compile arg = do
  ps <- parseProgram arg prelude
  let s = execState typeCheck ps
  genCode (errors s) (varEnv s) (expEnv s) (typeEnv s) arg  
    
-- CODE GEN
genCode :: Errors -> VarEnv -> ExpEnv -> TypeEnv -> FilePath -> IO (Bool, String)
genCode err venv eenv cenv path
  | null err  = do
      genProgram venv eenv cenv path
      return (True, "")
  | otherwise =
      return (False, intercalate "\n" err)

-- codeGen :: VarEnv -> ExpEnv -> TypeEnv -> KindEnv ->
--            FilePath -> IO (Bool, HaskellCode)
-- codeGen venv eenv cenv kenv path = do
--   genProgram venv eenv cenv kenv path
--   return (True, "")
