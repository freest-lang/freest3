{-|
Module      :  FreestState
Description :  The FreeST state
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Utils.FreestState
( -- State
FreestState
, FreestS(..)
, initialState
-- Monad & map
, tMapM
, tMapWithKeyM
-- Variable environment
, getVenv
, getFromVenv
, setVenv
, addToVenv
, removeFromVenv
-- Type environment
, getTenv
, addToTenv
, getFromTenv
-- Expression environment
, getEenv
, addToEenv
-- Program variables (parsing only)
, newPVar
, getPVar
, rmPVar
-- Type variables (parsing only)
, newTVar
, getTVar
, rmTVar
-- Errors
, Errors (..)
, addError
, getFileName
) where

import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Bind
import           Parse.Lexer (Pos,defaultPos)
import           Utils.Errors
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Traversable

-- | The typing state
type Errors = [String]

data FreestS = FreestS {
  filename :: String
, varEnv   :: VarEnv
, expEnv   :: ExpEnv
, typeEnv  :: TypeEnv
, errors   :: Errors
, lastVar  :: Int
, pVarsInScope :: Map.Map String [PVar]
, tVarsInScope :: Map.Map String [TVar] -- TODO: merge
}

type FreestState = State FreestS

-- | Initial State

initialState :: String -> FreestS
initialState f = FreestS {
  filename = f
, varEnv   = Map.empty
, expEnv   = Map.empty
, typeEnv  = Map.empty
, errors   = []
, lastVar  = 0
, pVarsInScope = Map.empty
, tVarsInScope = Map.empty
}

-- | FILE NAME

getFileName :: FreestState String
getFileName = do
  s <- get
  return $ filename s

-- | VAR ENV

getVenv :: FreestState VarEnv
getVenv = do
  s <- get
  return $ varEnv s

getFromVenv :: PBind -> FreestState (Maybe TypeScheme)
getFromVenv x = do
  venv <- getVenv
  return $ venv Map.!? x

removeFromVenv :: PBind -> FreestState ()
removeFromVenv b = modify (\s -> s {varEnv= Map.delete b (varEnv s)})  

addToVenv :: PBind -> TypeScheme -> FreestState ()
addToVenv b t =
  modify (\s -> s{varEnv = Map.insert b t (varEnv s)})

venvMember :: PBind -> FreestState Bool
venvMember x = do
  venv <- getVenv
  return $ Map.member x venv

setVenv :: VarEnv -> FreestState ()
setVenv venv = modify (\s -> s{varEnv = venv})

-- | EXP ENV

getEenv :: FreestState ExpEnv
getEenv = do
  s <- get
  return $ expEnv s

-- Unsafe - must exist
getFromEenv :: PBind -> FreestState Expression
getFromEenv x = do
  eenv <- getEenv
  return $ eenv Map.! x

addToEenv :: PBind -> Expression -> FreestState ()
addToEenv k v =
  modify (\s -> s{expEnv=Map.insert k v (expEnv s)})    
     
-- | TYPE ENV

getTenv :: FreestState TypeEnv
getTenv = do
  s <- get
  return $ typeEnv s

addToTenv :: TBind -> Kind -> TypeScheme -> FreestState ()
addToTenv b k t =
  modify (\s -> s{typeEnv = Map.insert b (k, t) (typeEnv s)})

getFromTenv :: TBind -> FreestState (Maybe (Kind, TypeScheme))
getFromTenv  b = do
  tenv <- getTenv
  return $ tenv Map.!? b

-- | ERRORS

addError :: Pos -> [String] -> FreestState ()
addError p e = do
  modify (\s -> s {errors=(errors s) ++ [styleError (filename s) p e]})  
  
addErrorList :: [String] -> FreestState ()
addErrorList es =
  modify (\s -> s {errors = (errors s) ++ es})   

-- | FRESH VARS

newPVar :: String -> FreestState PVar
newPVar id = do
  s <- get
  let pvar = mkPVar (lastVar s) id
  put $ s {lastVar = lastVar s + 1, pVarsInScope = Map.insertWith (++) id [pvar] (pVarsInScope s)}
  return pvar

getPVar :: String -> FreestState PVar
getPVar id = do
  s <- get
  case (pVarsInScope s) Map.!? id of
    Just pvar -> return $ head $ pvar
    Nothing   -> newPVar id >>= \pvar -> return pvar

rmPVar :: PBind -> FreestState ()
rmPVar (PBind _ pvar) = do
  s <- get
  put $ s {pVarsInScope = Map.update tailMaybe (show pvar) (pVarsInScope s)}
  where tailMaybe []     = Nothing
        tailMaybe (_:xs) = Just xs

newTVar :: String -> FreestState TVar
newTVar id = do
  s <- get
  let pvar = mkTVar (lastVar s) id
  put $ s {lastVar = lastVar s + 1, tVarsInScope = Map.insertWith (++) id [pvar] (tVarsInScope s)}
  return pvar

getTVar :: String -> FreestState TVar
getTVar id = do
  s <- get
  return $ head $ (tVarsInScope s) Map.! id

rmTVar :: PBind -> FreestState ()
rmTVar (PBind _ pvar) = do
  s <- get
  put $ s {tVarsInScope = Map.update tailMaybe (show pvar) (tVarsInScope s)}
  where tailMaybe []     = Nothing
        tailMaybe (_:xs) = Just xs

-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

