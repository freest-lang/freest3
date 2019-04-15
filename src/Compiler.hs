module Compiler (compile) where

import Syntax.Schemes (TypeEnv, VarEnv)
import Syntax.Expressions (ExpEnv)
import Validation.Rename (renameState)
import Validation.TypeChecking (typeCheck)
import CodeGen.CodeGen (genProgram)
import Parse.Parser (parseProgram)
import Utils.PreludeLoader (prelude)
import Utils.FreestState
import Control.Monad.State
import Data.List (intercalate)


compile :: String -> IO (Bool, String)
compile sourceFile = do
  state <- parseProgram sourceFile prelude
  let s' = execState renameState state
  let s = execState typeCheck s'
  genCode (errors s) (varEnv s) (expEnv s) (typeEnv s) sourceFile  
    
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
