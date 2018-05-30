{-# LANGUAGE OverloadedStrings #-}
module Compiler (compile) where

import           Control.Concurrent.Chan.Synchronous
import qualified Data.Map.Strict as Map
import           PreludeLoader
import           Terms.Parser
import           Terms.Terms
import           CodeGen.CodeGen
import           Types.Types
import           TypeChecking.TypeChecking
import           Control.Monad.Writer
import           Control.Monad.State
import           Data.List
import           System.Exit
import           Types.Kinds
import           Types.Kinding

compile :: String -> IO (Bool, String)
compile arg = do
  prog <- mainProgram arg prelude
  
  case prog of
    Right (venv, eenv, cenv, kenv) -> do
     -- error $ show eenv
      let a = typeCheck venv eenv cenv kenv
      if typeChecks a then        
        codeGen venv eenv cenv kenv (reverse $ dropWhile (/= '/') (reverse arg))
      else
        checkErr a
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

typeChecks :: TCheckM () -> Bool
typeChecks = null . snd . runWriter

showErrors :: TCheckM () -> [String]
showErrors = snd . runWriter


checkErr :: TCheckM () -> IO (Bool, String)
checkErr tc = do
  return (False, intercalate "\n" (showErrors tc))
