module Compiler (compile) where

import Syntax.Programs
import Syntax.Kinds
import CodeGen.CodeGen (genProgram)
import Control.Monad.State
import Data.List (intercalate)
import Parse.Parser (parseProgram)
import Utils.PreludeLoader (prelude)
import Utils.FreestState
import Validation.Typing (typeCheck)


compile :: String -> IO (Bool, String)
compile arg = do
  ps <- parseProgram arg prelude
  let s = execState typeCheck ps
  genCode (errors s) (varEnv s) (expEnv s) (consEnv s) (kindEnv s) arg  
    
-- CODE GEN
genCode :: Errors -> VarEnv -> ExpEnv -> TypeEnv -> KindEnv -> FilePath -> IO (Bool, String)
genCode err venv eenv cenv kenv path
  | null err  = do
      genProgram venv eenv cenv kenv path
      return (True, "")
  | otherwise =
      return (False, intercalate "\n\n" err)

-- codeGen :: VarEnv -> ExpEnv -> TypeEnv -> KindEnv ->
--            FilePath -> IO (Bool, HaskellCode)
-- codeGen venv eenv cenv kenv path = do
--   genProgram venv eenv cenv kenv path
--   return (True, "")
