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
( FreestState
, FreestS(..)
, Errors (..)
, tMapM
, tMapWithKeyM
, getFromVenv
, getTenv
, getVenv
, getEenv
, setVenv
, addToTenv
, addToVenv
, addToEenv
, removeFromVenv
, getFromTenv
, getFileName
, initialState
--, newVar
, getPvar
, newPVar
, rmPVar
, addError
) where

import           Parse.Lexer (Pos)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Bind
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
, varsInScope :: Map.Map String [PVar]
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
, varsInScope = Map.empty
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
removeFromVenv x =
  modify (\s -> s {varEnv= Map.delete x (varEnv s)})  

addToVenv :: PBind -> TypeScheme -> FreestState ()
addToVenv b t =
  modify (\s -> s{varEnv=Map.insert b t (varEnv s)})

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

{- Kind environments are now passed as parameter

-- | KIND ENV

getKenv :: FreestState KindEnv
getKenv = do
  s <- get
  return $ kindEnv s

addToKenv :: TBind -> Kind -> FreestState ()
addToKenv x k =
  modify (\s -> s {kindEnv=Map.insert x k (kindEnv s)})

kenvMember :: TBind -> FreestState Bool
kenvMember x = do
  kenv <- getKenv
  return $ Map.member x kenv

getKind :: TBind -> FreestState Kind -- Remove ?
getKind x = do
  kenv <- getKenv
  return $ kenv Map.! x

getFromKenv :: TBind -> FreestState (Maybe Kind)
getFromKenv x = do
  kenv <- getKenv
  return $ kenv Map.!? x   

removeFromKenv :: TBind -> FreestState ()
removeFromKenv x =
  modify (\s -> s {kindEnv = Map.delete x (kindEnv s)})

resetKEnv ::  FreestState ()
resetKEnv = modify (\s -> s {kindEnv = Map.empty})
-}

-- | ERRORS

addError :: Pos -> [String] -> FreestState ()
addError p e = do
  modify (\s -> s {errors=(errors s) ++ [styleError (filename s) p e]})  
  
addErrorList :: [String] -> FreestState ()
addErrorList es =
  modify (\s -> s {errors = (errors s) ++ es})   

-- | FRESH VARS

newPVar :: String -> FreestState PVar
newPVar x = do
  s <- get
  let y = PVar $ show (lastVar s) ++ ('_' : x)
  put $ s {lastVar = lastVar s + 1, varsInScope = Map.insertWith (++) x [y] (varsInScope s)}
  return y

getPvar :: String -> FreestState PVar
getPvar x = do
  s <- get
  return $ head $ (varsInScope s) Map.! x

rmPVar :: String -> FreestState ()
rmPVar x = do
  s <- get
  put $ s {varsInScope = Map.update tailMaybe x (varsInScope s)}
  where tailMaybe []    = Nothing
        tailMaybe (_:xs) = Just xs

-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

