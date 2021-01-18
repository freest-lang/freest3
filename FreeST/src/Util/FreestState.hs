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

module Util.FreestState
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
-- Program
  , getProg
  , getFromProg
  , addToProg
  , setProg
-- Errors
  , Errors
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
  , debugM
-- Parse Env
  , ParseEnv
  , emptyPEnv
  , addToPEnv
  , getPEnv 
  , setPEnv 
  )
where

import           Control.Monad.State
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.Error
-- import qualified Data.Set as Set
import qualified Data.Traversable              as Traversable
import           Util.ErrorMessage
import           Debug.Trace -- debug (used on debugM function)

-- | The typing state

-- type Errors = Set.Set String
type Errors = [String]

type ParseEnv = Map.Map ProgVar ([ProgVar], Exp)

data FreestS = FreestS {
  filename  :: String
, varEnv    :: VarEnv
, prog      :: Prog
, typeEnv   :: TypeEnv
, typenames :: TypeOpsEnv
, errors    :: Errors
, nextIndex :: Int
, parseEnv  :: ParseEnv
} deriving Show -- FOR DEBUG purposes

type FreestState = State FreestS

-- | Initial State

initialState :: String -> FreestS
initialState f = FreestS { filename  = f
                         , varEnv    = Map.empty
                         , prog      = Map.empty
                         , typeEnv   = Map.empty
                         , typenames = Map.empty
                         , errors    = []
                         , nextIndex = 0
                         , parseEnv  = Map.empty
                         }

-- | Parse Env

emptyPEnv :: FreestS -> FreestS
emptyPEnv s = s { parseEnv = Map.empty }

addToPEnv :: ProgVar -> [ProgVar] -> Exp -> FreestState ()
addToPEnv x xs e =
  modify (\s -> s { parseEnv = Map.insert x (xs, e) (parseEnv s) })

getPEnv :: FreestState ParseEnv
getPEnv = gets parseEnv

setPEnv :: ParseEnv -> FreestState ()
setPEnv pEnv = modify (\s -> s { parseEnv = pEnv })


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

getFromVEnv :: ProgVar -> FreestState (Maybe T.Type)
getFromVEnv x = do
  vEnv <- getVEnv
  return $ vEnv Map.!? x

removeFromVEnv :: ProgVar -> FreestState ()
removeFromVEnv b = modify (\s -> s { varEnv = Map.delete b (varEnv s) })

addToVEnv :: ProgVar -> T.Type -> FreestState ()
addToVEnv b t = modify (\s -> s { varEnv = Map.insert b t (varEnv s) })

-- vEnvMember :: ProgVar -> FreestState Bool
-- vEnvMember x = Map.member x <$> getVEnv

setVEnv :: VarEnv -> FreestState ()
setVEnv vEnv = modify (\s -> s { varEnv = vEnv })

-- | EXP ENV

getProg :: FreestState Prog
getProg = gets prog

getFromProg :: ProgVar -> FreestState (Maybe Exp)
getFromProg x = do
  eEnv <- getProg
  return $ eEnv Map.!? x

addToProg :: ProgVar -> Exp -> FreestState ()
addToProg k v = modify (\s -> s { prog = Map.insert k v (prog s) })

setProg :: Prog -> FreestState ()
setProg p = modify (\s -> s { prog = p })

-- | TYPE ENV

getTEnv :: FreestState TypeEnv
getTEnv = gets typeEnv

addToTEnv :: TypeVar -> Kind -> T.Type -> FreestState ()
addToTEnv x k t =
  modify (\s -> s { typeEnv = Map.insert x (k, t) (typeEnv s) })

getFromTEnv :: TypeVar -> FreestState (Maybe (Kind, T.Type))
getFromTEnv b = do
  tEnv <- getTEnv
  return $ tEnv Map.!? b

setTEnv :: TypeEnv -> FreestState ()
setTEnv tEnv = modify (\s -> s { typeEnv = tEnv })

-- | TYPENAMES

addTypeName :: Pos -> T.Type -> FreestState ()
addTypeName p t = modify (\s -> s { typenames = Map.insert p t (typenames s) })

getTypeNames :: FreestState TypeOpsEnv
getTypeNames = gets typenames

findTypeName :: Pos -> T.Type -> FreestState T.Type
findTypeName p t = Map.findWithDefault t p <$> getTypeNames

-- | ERRORS

getErrors :: FreestS -> String
getErrors = intercalate "\n" . take 10 . errors

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

-- | Debug Function

debugM :: String -> FreestState ()
debugM err = do
  i <- getNextIndex
  traceM $ "\n" ++ show i ++ ". " ++ err ++ "\n"
