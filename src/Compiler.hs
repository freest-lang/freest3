module Compiler (compile) where

import qualified Data.Map.Strict as Map
import           Utils.PreludeLoader
import           Parse.Parser (parseProgram)
import           Syntax.Exps
import           CodeGen.CodeGen
import           Syntax.Types
import           Validation.Typing
import           Validation.TypingState
import           Control.Monad.State
import           Data.List
import           System.Exit
import           Syntax.Kinds
import           Validation.Kinding


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
