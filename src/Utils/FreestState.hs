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
-- Next index
, getNextIndex
-- Variable environment
, getVEnv
, getFromVEnv
, addToVEnv
, setVEnv
, removeFromVEnv
-- Type environment
, getTEnv
, getFromTEnv
, addToTEnv
, setTEnv
-- Expression environment
, getEEnv
, getFromEEnv
, addToEEnv
, setEEnv
-- Errors
, Errors (..)
, addError
, getFileName
) where

import           Syntax.Expressions
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Utils.Errors
import           Control.Monad.State
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Traversable as Traversable

-- | The typing state

type Errors = [String]
-- type Errors = Set.Set String

data FreestS = FreestS {
  filename :: String
, varEnv   :: VarEnv
, expEnv   :: ExpEnv
, typeEnv  :: TypeEnv
, errors   :: Errors
, nextIndex  :: Int
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
, nextIndex  = 0
}

-- | NEXT VAR

getNextIndex :: FreestState Int
getNextIndex = do
  s <- get
  let next = nextIndex s
  modify (\s -> s{nextIndex = next + 1})
  return next

-- | FILE NAME

getFileName :: FreestState String
getFileName = do
  s <- get
  return $ filename s

-- | VAR ENV

getVEnv :: FreestState VarEnv
getVEnv = do
  s <- get
  return $ varEnv s

getFromVEnv :: ProgVar -> FreestState (Maybe TypeScheme)
getFromVEnv x = do
  vEnv <- getVEnv
  return $ vEnv Map.!? x

removeFromVEnv :: ProgVar -> FreestState ()
removeFromVEnv b = modify (\s -> s {varEnv= Map.delete b (varEnv s)})  

addToVEnv :: ProgVar -> TypeScheme -> FreestState ()
addToVEnv b t =
  modify (\s -> s{varEnv = Map.insert b t (varEnv s)})

vEnvMember :: ProgVar -> FreestState Bool
vEnvMember x = do
  vEnv <- getVEnv
  return $ Map.member x vEnv

setVEnv :: VarEnv -> FreestState ()
setVEnv vEnv = modify (\s -> s{varEnv = vEnv})

-- | EXP ENV

getEEnv :: FreestState ExpEnv
getEEnv = do
  s <- get
  return $ expEnv s

getFromEEnv :: ProgVar -> FreestState (Maybe Expression)
getFromEEnv x = do
  eEnv <- getEEnv
  return $ eEnv Map.!? x

addToEEnv :: ProgVar -> Expression -> FreestState ()
addToEEnv k v =
  modify (\s -> s{expEnv=Map.insert k v (expEnv s)})

setEEnv :: ExpEnv -> FreestState ()
setEEnv eEnv = modify (\s -> s{expEnv = eEnv})

-- | TYPE ENV

getTEnv :: FreestState TypeEnv
getTEnv = do
  s <- get
  return $ typeEnv s

addToTEnv :: TypeVar -> Kind -> TypeScheme -> FreestState ()
addToTEnv x k t =
  modify (\s -> s{typeEnv = Map.insert x (k, t) (typeEnv s)})

getFromTEnv :: TypeVar -> FreestState (Maybe (Kind, TypeScheme))
getFromTEnv  b = do
  tEnv <- getTEnv
  return $ tEnv Map.!? b

setTEnv :: TypeEnv -> FreestState ()
setTEnv tEnv = modify (\s -> s{typeEnv = tEnv})

-- | ERRORS

addError :: Pos -> [String] -> FreestState ()
addError p e = do
  modify (\s -> s{errors=(errors s) ++ [styleError (filename s) p e]})  
  -- modify (\s -> s{errors = Set.insert (styleError (filename s) p e) (errors s)})
  
-- addErrorList :: [String] -> FreestState ()
-- addErrorList es =
--   modify (\s -> s {errors = (errors s) ++ es})

-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

{- An attempt to rename at parsing time

newPVar :: String -> FreestState PVar
newPVar id = do
  s <- get
  let pvar = mkPVar (nextIndex s) id
  put $ s {nextIndex = nextIndex s + 1, pVarsInScope = Map.insertWith (++) id [pvar] (pVarsInScope s)}
  return pvar

getPVar :: String -> FreestState PVar
getPVar id = do
  s <- get
  case (pVarsInScope s) Map.!? id of
    Just pvar -> return $ head $ pvar
    Nothing   -> newPVar id >>= \pvar -> return pvar

rmPVar :: ProgVar -> FreestState ()
rmPVar (ProgVar _ pvar) = do
  s <- get
  put $ s {pVarsInScope = Map.update tailMaybe (show pvar) (pVarsInScope s)}
  where tailMaybe []     = Nothing
        tailMaybe (_:xs) = Just xs

newTVar :: Pos -> String -> FreestState TypeVar
newTVar p id = do
  s <- get
  let x = newTypeVar p (nextIndex s) id
  put s {nextIndex      = nextIndex s + 1,
         tTable = Map.insertWith (++) id [x] (tTable s),
         tStack       = id : tStack s}
  return x

getTVar :: Pos -> String -> FreestState TypeVar
getTVar p id = do
  s <- get
  return $ head $ Map.findWithDefault [mkTypeVar p id] id (tTable s)

mark :: String
mark = "#"

beginScope :: FreestState ()
beginScope = modify (\s -> s {tStack = mark : tStack s})

endScope :: FreestState ()
endScope = do
  s <- get
  let (t, m) = remove (tStack s) (tTable s)
  put s {tStack = t, tTable = m}
  where
  remove (x:xs) m
    | x == mark = (xs, m)
    | otherwise = remove xs (Map.update tailMaybe x m)
  tailMaybe []     = Nothing
  tailMaybe (_:xs) = Just xs

newTVar :: String -> FreestState TVar
newTVar id = do
  s <- get
  let pvar = mkTVar (nextIndex s) id
  put $ s {nextIndex = nextIndex s + 1, tTable = Map.insertWith (++) id [pvar] (tTable s)}
  return pvar

getTVar :: String -> FreestState TVar
getTVar id = do
  s <- get
  return $ head $ (tTable s) Map.! id

rmTVar :: ProgVar -> FreestState ()
rmTVar (ProgVar _ pvar) = do
  s <- get
  put $ s {tTable = Map.update tailMaybe (show pvar) (tTable s)}
  where tailMaybe []     = Nothing
        tailMaybe (_:xs) = Just xs
-}
