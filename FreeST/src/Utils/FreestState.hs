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
  , Errors(..)
  , getErrors
  , addError
  , hasErrors
  , ErrorMessage(..)
  , ErrorMsg(..)
  , getFileName
-- Typenames
  , addTypeName
  , getTypeNames
  , findTypeName
  )
where

import           Control.Monad.State
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import           Syntax.Expressions
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.Schemes
import           Syntax.TypeVariables
import           Syntax.Types                   ( Type(..)
                                                , TypeOpsEnv
                                                )
import           Utils.Errors
-- import qualified Data.Set as Set
import qualified Data.Traversable              as Traversable
import           Utils.ErrorMessage

-- | The typing state

-- type Errors = Set.Set String
type Errors = [String]

data FreestS = FreestS {
  filename  :: String
, varEnv    :: VarEnv
, expEnv    :: ExpEnv
, typeEnv   :: TypeEnv
, typenames :: TypeOpsEnv
, errors    :: Errors
, nextIndex :: Int
}

type FreestState = State FreestS

-- | Initial State

initialState :: String -> FreestS
initialState f = FreestS { filename  = f
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
  modify (\s -> s { nextIndex = next + 1 })
  return next

-- | FILE NAME

getFileName :: FreestState String
getFileName = gets filename

-- | VAR ENV

getVEnv :: FreestState VarEnv
getVEnv = gets varEnv

getFromVEnv :: ProgVar -> FreestState (Maybe TypeScheme)
getFromVEnv x = do
  vEnv <- getVEnv
  return $ vEnv Map.!? x

removeFromVEnv :: ProgVar -> FreestState ()
removeFromVEnv b = modify (\s -> s { varEnv = Map.delete b (varEnv s) })

addToVEnv :: ProgVar -> TypeScheme -> FreestState ()
addToVEnv b t = modify (\s -> s { varEnv = Map.insert b t (varEnv s) })

vEnvMember :: ProgVar -> FreestState Bool
vEnvMember x = Map.member x <$> getVEnv

setVEnv :: VarEnv -> FreestState ()
setVEnv vEnv = modify (\s -> s { varEnv = vEnv })

-- | EXP ENV

getEEnv :: FreestState ExpEnv
getEEnv = gets expEnv

getFromEEnv :: ProgVar -> FreestState (Maybe Expression)
getFromEEnv x = do
  eEnv <- getEEnv
  return $ eEnv Map.!? x

addToEEnv :: ProgVar -> Expression -> FreestState ()
addToEEnv k v = modify (\s -> s { expEnv = Map.insert k v (expEnv s) })

setEEnv :: ExpEnv -> FreestState ()
setEEnv eEnv = modify (\s -> s { expEnv = eEnv })

-- | TYPE ENV

getTEnv :: FreestState TypeEnv
getTEnv = gets typeEnv

addToTEnv :: TypeVar -> Kind -> TypeScheme -> FreestState ()
addToTEnv x k t =
  modify (\s -> s { typeEnv = Map.insert x (k, t) (typeEnv s) })

getFromTEnv :: TypeVar -> FreestState (Maybe (Kind, TypeScheme))
getFromTEnv b = do
  tEnv <- getTEnv
  return $ tEnv Map.!? b

setTEnv :: TypeEnv -> FreestState ()
setTEnv tEnv = modify (\s -> s { typeEnv = tEnv })

-- | TYPENAMES

addTypeName :: Pos -> Type -> FreestState ()
addTypeName p t = modify (\s -> s { typenames = Map.insert p t (typenames s) })

getTypeNames :: FreestState TypeOpsEnv
getTypeNames = gets typenames

findTypeName :: Pos -> Type -> FreestState Type
findTypeName p t = Map.findWithDefault t p <$> getTypeNames

-- | ERRORS

getErrors :: FreestS -> String
getErrors = intercalate "\n" . errors

hasErrors :: FreestS -> Bool
hasErrors = not . null . errors

addError :: Pos -> [ErrorMessage] -> FreestState ()
addError p em = do
  f    <- getFileName
  tops <- getTypeNames
  let es = formatErrorMessages tops p f em
  modify (\s -> s { errors = insertError (errors s) es })

insertError :: [String] -> String -> [String]
insertError es err | err `elem` es = es
                   | otherwise     = es ++ [err]

-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapM_ :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m ()
tMapM_ f m = void $ tMapM f m

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

tMapWithKeyM_ :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m ()
tMapWithKeyM_ f m = void $ tMapWithKeyM f m
