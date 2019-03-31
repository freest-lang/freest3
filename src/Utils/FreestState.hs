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

module Utils.FreestState where

import           Parse.Lexer (Pos)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Utils.Errors

-- | The typing state
type Errors = [String]

data FreestS = FreestS {
  filename :: String,
  varEnv :: VarEnv,
  expEnv :: ExpEnv,
  consEnv :: TypeEnv,
  kindEnv :: KindEnv,
  errors :: Errors,
  fv :: Int}

type FreestState = State FreestS

-- | Initial State

initialState :: String -> FreestS
initialState f = FreestS {filename = f,
                          varEnv = Map.empty,
                          expEnv = Map.empty,
                          consEnv = Map.empty,
                          kindEnv = Map.empty,
                          errors = [],
                          fv = 0}

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

getFromVenv :: Bind -> FreestState (Maybe TypeScheme)
getFromVenv x = do
  venv <- getVenv
  return $ venv Map.!? x

removeFromVenv :: Bind -> FreestState ()
removeFromVenv x =
  modify (\s -> s {varEnv= Map.delete x (varEnv s)})  

addToVenv :: Bind -> TypeScheme -> FreestState ()
addToVenv b t =
  modify (\s -> s{varEnv=Map.insert b t (varEnv s)})

venvMember :: Bind -> FreestState Bool
venvMember x = do
  venv <- getVenv
  return $ Map.member x venv

setVenv :: VarEnv -> FreestState ()
setVenv venv = modify (\s -> s{varEnv=venv})

-- | EXP ENV

getEenv :: FreestState ExpEnv
getEenv = do
  s <- get
  return $ expEnv s

-- Unsafe - must exist
getFromEenv :: Bind -> FreestState ([Bind], Expression)
getFromEenv x = do
  eenv <- getEenv
  return $ eenv Map.! x

addToEenv :: Bind -> ([Bind], Expression) -> FreestState ()
addToEenv k v =
  modify (\s -> s{expEnv=Map.insert k v (expEnv s)})    
     
-- | TYPE ENV

getTenv :: FreestState TypeEnv
getTenv = do
  s <- get
  return $ consEnv s

addToTenv :: Bind -> Kind -> TypeScheme -> FreestState ()
addToTenv b k t =
  modify (\s -> s{consEnv = Map.insert b (k, t) (consEnv s)})

getFromTenv :: Bind -> FreestState (Maybe (Kind, TypeScheme))
getFromTenv  b = do
  tenv <- getTenv
  return $ tenv Map.!? b

-- | KIND ENV

getKenv :: FreestState KindEnv
getKenv = do
  s <- get
  return $ kindEnv s

addToKenv :: Bind -> Kind -> FreestState ()
addToKenv x k =
  modify (\s -> s {kindEnv=Map.insert x k (kindEnv s)})

kenvMember :: Bind -> FreestState Bool
kenvMember x = do
  kenv <- getKenv
  return $ Map.member x kenv

getKind :: Bind -> FreestState Kind -- Remove ?
getKind x = do
  kenv <- getKenv
  return $ kenv Map.! x

getFromKenv :: Bind -> FreestState (Maybe Kind)
getFromKenv x = do
  kenv <- getKenv
  return $ kenv Map.!? x   

removeFromKenv :: Bind -> FreestState ()
removeFromKenv x =
  modify (\s -> s {kindEnv = Map.delete x (kindEnv s)})

resetKEnv ::  FreestState ()
resetKEnv = modify (\s -> s {kindEnv = Map.empty})

-- | ERRORS

addError :: Pos -> [String] -> FreestState ()
addError p e = do
  modify (\s -> s {errors=(errors s) ++ [styleError (filename s) p e]})  
  
addErrorList :: [String] -> FreestState ()
addErrorList es =
  modify (\s -> s {errors=(errors s) ++ es})   


-- | FRESH VARS

freshVar :: FreestState String
freshVar = do
  s <- get
  put $ s {fv=(fv s) +1}
  return $ "_x" ++ (show (fv s))
