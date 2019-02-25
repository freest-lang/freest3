module Validation.TypingState  where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Utils.Errors

type KindEnv = Map.Map TypeVar (Pos, Kind)

-- | The typing state
type Errors = [String]
type TypingState = State (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors)
  
-- | State manipulating functions

-- | Initial State
-- (_,_,_,_,_,_)
-- (f, venv, eenv, cenv, kenv, err)
initialState :: String -> (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors)
initialState f = (f, Map.empty, Map.empty, Map.empty, Map.empty, [])

-- | FILE NAME

getFileName :: TypingState String
getFileName = do
  (f,_,_,_,_,_) <- get
  return f

-- | VAR ENV

getVenv :: TypingState VarEnv
getVenv = do
  (_,venv,_,_,_,_) <- get
  return venv

getFromVenv :: TermVar -> TypingState (Pos, TypeScheme)
getFromVenv x = do
  venv <- getVenv
  return $ venv Map.! x

removeFromVenv :: TermVar -> TypingState ()
removeFromVenv x =
  modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, Map.delete x venv, eenv, cenv, kenv, err))
  
addToVenv :: Pos -> TermVar -> TypeScheme -> TypingState ()
addToVenv p x t =
  modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, Map.insert x (p, t) venv, eenv, cenv, kenv, err))

venvMember :: TermVar -> TypingState Bool
venvMember x = do
  venv <- getVenv
  return $ Map.member x venv

setVenv :: VarEnv -> TypingState ()
setVenv venv = modify (\(f, _, eenv, cenv, kenv, err) ->
                         (f, venv, eenv, cenv, kenv, err))

-- | EXP ENV

getEenv :: TypingState ExpEnv
getEenv = do
  (_,_,eenv,_,_,_) <- get
  return eenv

-- Unsafe - must exist
getFromEenv :: TermVar -> TypingState (Pos, Params, Expression)
getFromEenv x = do
  eenv <- getEenv
  return $ eenv Map.! x
  

-- | CONSTRUCTOR ENV
getCenv :: TypingState ConstructorEnv
getCenv = do
  (_,_,_,cenv,_,_) <- get
  return cenv

-- | KIND ENV
getKenv :: TypingState KindEnv
getKenv = do
  (_,_,_,_,kenv,_) <- get
  return kenv

addToKenv :: Pos -> TypeVar -> Kind -> TypingState ()
addToKenv p x k =
  modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, venv, eenv, cenv, Map.insert x (p,k) kenv, err))

kenvMember :: TypeVar -> TypingState Bool
kenvMember x = do
  kenv <- getKenv
  return $ Map.member x kenv

getKind :: TypeVar -> TypingState Kind
getKind x = do
  kenv <- getKenv
  let (_,k) = kenv Map.! x
  return k 

removeFromKenv :: TypeVar -> TypingState ()
removeFromKenv x = do
  kenv <- getKenv
  if Map.member x kenv then
    modify (\(f, venv, eenv, cenv, kenv, err) ->
              (f, venv, eenv, cenv, Map.delete x kenv, err))
  else
    return ()      


-- ERRORS

addError :: Pos -> [String] -> TypingState ()
addError p es = do
  file <- getFileName 
  modify (\(f, venv, eenv, cenv, kenv, e) ->
            (f, venv, eenv, cenv, kenv,  e ++ [styleError file p es]))

addErrorList :: [String] -> TypingState ()
addErrorList ers =
   modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, venv, eenv, cenv, kenv, err ++ ers))



-- getErrors :: TypingState Errors
-- getErrors = do
--   (_ , _, err) <- get
--   return err


