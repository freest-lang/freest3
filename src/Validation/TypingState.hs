{-|
Module      :  Typing
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.TypingState where

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Utils.Errors

-- | The typing state
type Errors = [String]
type TypingState = State (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors, Int) -- TODO: use a record
  
-- | State manipulating functions

-- | Initial State
-- (_,_,_,_,_,_)
-- (f, venv, eenv, cenv, kenv, err, n)
initialState :: String ->
                 (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors, Int)
initialState f = (f, Map.empty, Map.empty, Map.empty, Map.empty, [], 0)

-- | FILE NAME

getFileName :: TypingState String
getFileName = do
  (f,_,_,_,_,_,_) <- get
  return f

-- | VAR ENV

getVenv :: TypingState VarEnv
getVenv = do
  (_,venv,_,_,_,_,_) <- get
  return venv

getFromVenv :: Bind -> TypingState TypeScheme
getFromVenv x = do
  venv <- getVenv
  return $ venv Map.! x

removeFromVenv :: Bind -> TypingState ()
removeFromVenv x =
  modify (\(f, venv, eenv, cenv, kenv, e, n) ->
            (f, Map.delete x venv, eenv, cenv, kenv, e, n))
  
addToVenv :: Bind -> TypeScheme -> TypingState ()
addToVenv b t =
  modify (\(f, venv, eenv, cenv, kenv, e, n) ->
            (f, Map.insert b t venv, eenv, cenv, kenv, e, n))

venvMember :: Bind -> TypingState Bool
venvMember x = do
  venv <- getVenv
  return $ Map.member x venv

setVenv :: VarEnv -> TypingState ()
setVenv venv = modify (\(f, _, eenv, cenv, kenv, e, n) ->
                         (f, venv, eenv, cenv, kenv, e, n))

-- | EXP ENV

getEenv :: TypingState ExpEnv
getEenv = do
  (_,_,eenv,_,_,_,_) <- get
  return eenv

-- Unsafe - must exist
getFromEenv :: Bind -> TypingState ([Bind], Expression)
getFromEenv x = do
  eenv <- getEenv
  return $ eenv Map.! x
  

-- | CONSTRUCTOR ENV
getCenv :: TypingState ConstructorEnv
getCenv = do
  (_,_,_,cenv,_,_,_) <- get
  return cenv

-- | KIND ENV
getKenv :: TypingState KindEnv
getKenv = do
  (_,_,_,_,kenv,_,_) <- get
  return kenv

addToKenv :: Bind -> Kind -> TypingState ()
addToKenv x k =
  modify (\(f, venv, eenv, cenv, kenv, e, n) ->
            (f, venv, eenv, cenv, Map.insert x k kenv, e, n))

kenvMember :: Bind -> TypingState Bool
kenvMember x = do
  kenv <- getKenv
  return $ Map.member x kenv

getKind :: Bind -> TypingState Kind
getKind x = do
  kenv <- getKenv
  return $ kenv Map.! x 

removeFromKenv :: Bind -> TypingState ()
removeFromKenv x = do
  kenv <- getKenv
  if Map.member x kenv then
    modify (\(f, venv, eenv, cenv, kenv, e, n) ->
              (f, venv, eenv, cenv, Map.delete x kenv, e, n))
  else
    return ()      


-- ERRORS

addError :: Pos -> [String] -> TypingState ()
addError p e = do
  file <- getFileName 
  modify (\(f, venv, eenv, cenv, kenv, e', n) ->
            (f, venv, eenv, cenv, kenv,  e' ++ [styleError file p e], n))

addErrorList :: [String] -> TypingState ()
addErrorList es =
   modify (\(f, venv, eenv, cenv, kenv, e, n) ->
            (f, venv, eenv, cenv, kenv, e ++ es, n))


-- FRESH VARS

freshVar :: TypingState String
freshVar = do
  (f, venv, eenv, cenv, kenv, e, n) <- get
  put (f, venv, eenv, cenv, kenv, e, n+1)
  return $ "_x" ++ show n
