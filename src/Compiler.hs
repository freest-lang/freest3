{-# LANGUAGE OverloadedStrings #-}
module Compiler (compile) where

-- import           Control.Concurrent.Chan.Synchronous
import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Parse.Parser
import           Syntax.Terms
import           CodeGen.CodeGen
import           Syntax.Types
import           Validation.Typing
import           Validation.TypingState
import           Control.Monad.Writer
import           Control.Monad.State
import           Data.List
import           System.Exit
import           Syntax.Kinds
import           Validation.Kinding

-- TODO

compile :: String -> IO (Bool, String)
compile arg = do
  prog <- mainProgram arg prelude
  
  case prog of
    Right (venv, eenv, cenv, kenv) ->
      do
--      error $ show venv ++ "\n\n\n" ++ show eenv
      -- let (x,y,z) = initialState
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
 -- let eenv1 = Map.delete "start" eenv
  genProgram venv eenv cenv kenv path
  -- writeFile (path ++ "cfst.hs") file
  return (True, "")


-- Functions to deal with typecheck monad

-- typeChecks :: TypingState () -> TypingState Bool
-- typeChecks :: TypingState Bool
-- typeChecks = do
--   xs <- getErrors
--   return $ null xs
  -- null . snd . runWriter

-- getErrors :: TypingState Errors
-- getErrors = do
--   (_ , _, err) <- get
--   return err
  

showErrors :: TypingState [String]
showErrors = getErrors -- snd . runWriter


-- checkErr :: TypingState () -> IO (Bool, String)
checkErr :: Errors -> IO (Bool, String)
checkErr tc = do
  return (False, intercalate "\n\n" tc)
