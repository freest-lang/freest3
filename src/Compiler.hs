module Compiler (compile) where

import Syntax.Programs
import Syntax.Exps
import Syntax.Types
import Syntax.Kinds
import CodeGen.CodeGen (HaskellCode, genProgram)
import Control.Monad.State
import Data.List (intercalate)
import Parse.Parser (parseProgram)
import System.Exit
import Utils.PreludeLoader (prelude)
import Utils.FreestState
import Validation.Typing (typeCheck)


compile :: String -> IO (Bool, String)
compile arg = do
  ps <- parseProgram arg prelude
  let s = execState typeCheck ps
  genCode (errors s) (varEnv s) (expEnv s) (consEnv s) (kindEnv s) arg  
    
-- CODE GEN
genCode :: Errors -> VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO (Bool, String)
genCode err venv eenv cenv kenv path
  | null err  = do
      genProgram venv eenv cenv kenv path
      return (True, "")
  | otherwise =
      return (False, intercalate "\n\n" err)

-- codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv ->
--            FilePath -> IO (Bool, HaskellCode)
-- codeGen venv eenv cenv kenv path = do
--   genProgram venv eenv cenv kenv path
--   return (True, "")
