module Compiler (compile) where

import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Parse.Parser (parseProgram)
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
--  prog <- mainProgram arg prelude
  bs@(f, venv, eenv, cenv, kenv, err) <- parseProgram arg -- TODO: REF prelude
    -- TODO: change (f, venv, eenv, cenv, kenv, err) with bs when parser accepts prelude
  let (_,_,_,_,_,ers) = execState typeCheck (f, Map.union venv prelude, eenv, cenv, kenv, err)
  checkErr ers venv eenv cenv kenv arg
    
    
  -- case prog of
  --   Right (venv, eenv, cenv, kenv) ->
  --     do
  --       let (_, _, errors) = execState (typeCheck eenv cenv) (kenv, venv, [])
  --       if null errors then        
  --         codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
  --       else
  --         checkErr errors
  --   Left err ->     
  --     return (False, show err)
 

-- CODE GEN

codeGen :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv ->
           FilePath -> IO (Bool, HaskellCode)
codeGen venv eenv cenv kenv path = do
  -- let start = eenv Map.! "start"
  genProgram venv eenv cenv kenv path
  return (True, "")
 

-- showErrors :: TypingState [String]
-- showErrors = getErrors

-- checkErr :: Errors -> IO (Bool, String)
-- checkErr tc = do
--   return (False, intercalate "\n\n" tc)

-- TODO: change reverse ...
checkErr :: Errors -> VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO (Bool, String)
checkErr err venv eenv cenv kenv arg
  | null err  = codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
  | otherwise = return (False, intercalate "\n\n" err)
