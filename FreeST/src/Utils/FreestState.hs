{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
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
, tMapM_
, tMapWithKeyM
, tMapWithKeyM_
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
, getErrors
, addError
, hasErrors
, ErrorMessage(..)
, getFileName
-- Typenames
, TypeNames
, addTypeName
, getTypeNames
, findTypeName
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Types (Type(..))
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Utils.Errors
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Data.List (intercalate)
-- import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import System.Console.Pretty (Color (..)) -- TODO: refactor

-- | The typing state

-- type Errors = Set.Set String
type Errors = [String]
type TypeNames = Map.Map Pos Type -- map between positions and type operators (typename, dualof)

data FreestS = FreestS {
  filename  :: String
, varEnv    :: VarEnv
, expEnv    :: ExpEnv
, typeEnv   :: TypeEnv
, typenames :: TypeNames
, errors    :: Errors
, nextIndex :: Int
}

type FreestState = State FreestS

-- | Initial State

initialState :: String -> FreestS
initialState f = FreestS {
  filename  = f
, varEnv    = Map.empty
, expEnv    = Map.empty
, typeEnv   = Map.empty
, typenames = Map.empty
, errors    = []
--, errors    = Set.empty
, nextIndex = 0
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

-- | TYPENAMES

addTypeName :: Pos -> Type -> FreestState ()
addTypeName p t = modify (\s -> s{typenames = Map.insert p t (typenames s)})

getTypeNames :: FreestState TypeNames
getTypeNames = do
  s <- get
  return $ typenames s

findTypeName :: Pos -> Type -> FreestState Type
findTypeName p t = do
  typenames <- getTypeNames
  return $ Map.findWithDefault t p typenames

-- | ERRORS

-- addError :: Pos -> [String] -> FreestState ()
-- addError p e =
--   modify (\s -> s{errors = insertError p (errors s) (filename s) e})
  
-- insertError :: Pos -> [String] -> String -> [String] -> [String]
-- insertError p es f e
--   | err `elem` es = es
--   | otherwise     = es ++ [err]
--   where
--     err = styleError f p e
          
getErrors :: FreestS -> String
getErrors = (intercalate "\n") . errors
  
hasErrors :: FreestS -> Bool
hasErrors = not . null . errors

-- | Error class and instances

-- TODO: move to other module (break cycles)

class ErrorMsg a where
  pos   :: a -> Pos -- Does not make sense to be here??
  msg   :: a -> FreestState String
  color :: a -> Maybe Color
  
instance ErrorMsg Type where
  pos     = position
  msg t   = liftM show (showM t)
  color _ = Just Red

instance ErrorMsg String where
  pos _   = defaultPos 
  msg s   = pure s
  color _ = Nothing
  
instance ErrorMsg Expression where
  pos     = position
  msg     = pure . show
  color _ = Just Red

instance ErrorMsg ProgVar where
  pos     = position
  msg     = pure . show
  color _ = Just Red

instance ErrorMsg TypeVar where
  pos     = position
  msg     = pure . show
  color _ = Just Red


instance ErrorMsg Pos where
  pos     = id
  msg     = pure . show
  color _ = Nothing

instance ErrorMsg Kind where
  pos     = position
  msg     = pure . show
  color _ = Just Red      

instance ErrorMsg TypeScheme where
  pos     = position
  msg     = pure . show
  color _ = Just Red      
  
  
data ErrorMessage where
  Error :: ErrorMsg a => a -> ErrorMessage

-- type ErrList = [ErrorMessage]

-- a :: Type -> Expression -> String -> ErrList
-- a t e s = [Error t, Error e, Error s]

-- addErr :: ErrList -> FreestState ()
-- addErr (Error x:xs) = do
-- --  str <- foldM (\acc (Error x)  -> liftM ((acc ++ " ") ++) (msg x)) "" xs
--   str <- foldM (\acc (Error x)  -> liftM ((acc ++ " ") ++) (msg x)) "" xs
--   s <- msg x
--   traceM $ "Position " ++ show (pos x) ++ " " ++ (s ++ str)
--   pure ()

formatErrorMessage :: Pos -> String -> [ErrorMessage] -> FreestState String
formatErrorMessage _ _ []     = pure ""
formatErrorMessage p fname es = do
  let header = styleHeader fname p
  body <- foldM (\acc e -> liftM ((acc ++ " ") ++) (formatError e)) "" es
  return $ header ++ body

formatError :: ErrorMessage -> FreestState String
formatError (Error e) =
  case color e of
    Just c  -> liftM (styleColor c) (formatErr e)
    Nothing -> formatErr e

formatErr :: ErrorMsg a => a -> FreestState String
formatErr m = liftM styleBold (msg m)

addError :: Pos -> [ErrorMessage] -> FreestState ()
addError p em = do
  s <- get
  es <- formatErrorMessage p (filename s) em
  modify (\s -> s{errors = insertError (errors s) es})
  
insertError :: [String] -> String -> [String]
insertError es err
  | err `elem` es = es
  | otherwise     = es ++ [err] 


-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapM_ :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m ()
tMapM_ f m = tMapM f m >> return ()

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

tMapWithKeyM_ :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m ()
tMapWithKeyM_ f m = tMapWithKeyM f m >> return ()


-- | Show types that consult the typename map

showM :: Type -> FreestState Type
showM s@(Semi p t u) = do
  tns <- getTypeNames
  case tns Map.!? p of
    Just t -> return t
    Nothing -> liftM2 (Semi p) (showM t) (showM u)
showM (Rec p xs t) = do
  tns <- getTypeNames
  case tns Map.!? p of
    Just t  -> return t
    Nothing -> liftM (Rec p xs) (showM t)
-- TODO:
-- showM (Fun p m t u) = 
-- showM (PairType p t u) = 
-- showM (Datatype p m) =  
showM (Choice p pol m) = do
  tns <- getTypeNames
  case tns Map.!? p of
    Just t  -> return t
    Nothing -> liftM (Choice p pol) (mapM showM m)
  
showM t = do
  tns <- getTypeNames
  return $ Map.findWithDefault t (position t) tns




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
