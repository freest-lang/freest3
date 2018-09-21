module Compiler (compile) where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Parse.Parser
import           Syntax.Terms
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
  prog <- mainProgram arg prelude
  
  case prog of
    Right (venv, eenv, cenv, kenv) ->
      do
        let (_, _, errors) = execState (typeCheck eenv cenv) (kenv, venv, [])
        if null errors then        
          codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
        else
          checkErr errors
    Left err ->     
      return (False, show err)
 

-- CODE GEN

codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv ->
           FilePath -> IO (Bool, HaskellCode)
codeGen venv eenv cenv kenv path = do
  let start = eenv Map.! "start"
  genProgram venv eenv cenv kenv path
  return (True, "")
 

showErrors :: TypingState [String]
showErrors = getErrors

checkErr :: Errors -> IO (Bool, String)
checkErr tc = do
  return (False, intercalate "\n\n" tc)
