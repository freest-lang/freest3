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
import Validation.Typing (typeCheck)
import Validation.TypingState (Errors)

compile :: String -> IO (Bool, String)
compile arg = do
  bs@(f, venv, eenv, cenv, kenv, err) <- parseProgram arg prelude
  let (_,_,_,_,_,ers, _) = execState typeCheck (f, venv, eenv, cenv, kenv, err, 0)
  checkErr ers venv eenv cenv kenv arg
    
-- CODE GEN

codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv ->
           FilePath -> IO (Bool, HaskellCode)
codeGen venv eenv cenv kenv path = do
  -- let start = eenv Map.! "start"
  genProgram venv eenv cenv kenv path
  return (True, "")
 
-- TODO: change reverse ... here and Main
checkErr :: Errors -> VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO (Bool, String)
checkErr err venv eenv cenv kenv arg
  | null err  = codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
  | otherwise = return (False, intercalate "\n\n" err)
