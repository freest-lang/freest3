module Validation.TypingState  where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types

type KindEnv = Map.Map TypeVar Kind

-- The typing state
type Errors = [String]
type TypingState = State (KindEnv, VarEnv, Errors)
  
-- State manipulating functions

-- Initial State

initialState :: (KindEnv, VarEnv, Errors)
initialState = (Map.empty, Map.empty, [])

-- Errors

addError :: String -> TypingState ()
addError err = modify (\(kenv, venv, errors) -> (kenv, venv, errors ++ [err]))

addErrorList :: [String] -> TypingState ()
addErrorList err = modify (\(kenv, venv, errors) -> (kenv, venv, errors ++ err)) 

-- getErrors :: TypingState Errors
-- getErrors = do
--   (_ , _, err) <- get
--   return err

-- VarEnv

getVarEnv :: TypingState VarEnv
getVarEnv = do
  (_ , venv, _) <- get
  return venv

getFromVarEnv :: TermVar -> TypingState TypeScheme
getFromVarEnv x = do
  venv <- getVarEnv
  return $ venv Map.! x

removeFromVarEnv :: TermVar -> TypingState ()
removeFromVarEnv x =
  modify (\(kenv, venv, errors) -> (kenv, Map.delete x venv, errors))
  
addToVEnv :: TermVar -> TypeScheme -> TypingState ()
addToVEnv x t =
  modify (\(kenv, venv, errors) -> (kenv, Map.insert x t venv, errors))

venvMember :: TermVar -> TypingState Bool
venvMember x = do
  venv <- getVarEnv
  return $ Map.member x venv

setVEnv :: VarEnv -> TypingState ()
setVEnv venv = modify (\(kenv, _, errors) -> (kenv, venv, errors))

-- KindEnv

getKindEnv :: TypingState KindEnv
getKindEnv = do
  (kenv, _, _) <- get
  return kenv

addToKenv :: TypeVar -> Kind -> TypingState ()
addToKenv x k = modify (\(kenv, venv, err) -> (Map.insert x k kenv, venv, err))

kenvMember :: TypeVar -> TypingState Bool
kenvMember x = do
  kenv <- getKindEnv
  return $ Map.member x kenv

getKind :: TypeVar -> TypingState Kind
getKind x = do
  kenv <- getKindEnv
  return $ kenv Map.! x

-- setKEnv :: KindEnv -> TypingState ()
-- setKEnv kenv = modify (\(_, venv, errors) -> (kenv, venv, errors))
