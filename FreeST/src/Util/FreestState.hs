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

module Util.FreestState (
-- * State
    FreestState
  , FreestS(..)
  , initialState
-- * Monad & map
  , tMapM
  , tMapM_
  , tMapWithKeyM
  , tMapWithKeyM_
-- * Next index
  , getNextIndex
  , freshTVar
-- * Variable environment
  , getVEnv
  , getFromVEnv
  , addToVEnv
  , setVEnv
  , removeFromVEnv
-- * Type environment
  , getTEnv
  , getFromTEnv
  , addToTEnv
  , setTEnv
-- * Program
  , getProg
  , getFromProg
  , addToProg
  , setProg
-- * Warnings
  --, Warnings
  , getWarnings
  , addWarning
  , hasWarnings
  --, WarningMessage(..)
  --, WarningMsg(..)
-- * Errors
  , Errors
  , getErrors
  , addError
  , hasErrors
  , ErrorMessage(..)
  , ErrorMsg(..)
  , getFileName
-- * Typenames
  , addTypeName
  , getTypeNames
  , findTypeName
  , addDualof
  , debugM
-- * Parse Env
  , ParseEnv
  , emptyPEnv
  , addToPEnv
  , getPEnv
  , setPEnv
-- * Run Options
  , RunOpts(..)
  , defaultOpts
  , initialOpts
  , getMain
  , isMainFlagSet
  , isQuietFlagSet
  , getOpts
  )
where

import           Control.Applicative
import           Control.Monad.State
import           Data.List ( intercalate ) -- , sortBy )
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Traversable as Traversable
import           Debug.Trace -- debug (used on debugM function)
import           Syntax.Base
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type as T
import           Syntax.TypeVariable
import           Util.Warning
import           Util.WarningMessage ()
import           Util.PrettyWarning ()
-- import           Util.PreludeLoader
import           Util.Error
import           Util.ErrorMessage
import           Util.PrettyError ()

-- | The typing state

type Warnings = [WarningType]

-- type Errors = Set.Set String
-- type Errors = [(Pos, String)]
type Errors = [ErrorType]

type ParseEnv = Map.Map ProgVar ([ProgVar], Exp)

data FreestS = FreestS {
  runOpts    :: RunOpts
, varEnv     :: VarEnv
, prog       :: Prog
, typeEnv    :: TypeEnv
, typenames  :: TypeOpsEnv
, warnings   :: Warnings
, errors     :: Errors
, nextIndex  :: Int
, parseEnv   :: ParseEnv -- "discarded" after elaboration
} deriving Show -- FOR DEBUG purposes

type FreestState = State FreestS

-- | Initial State

initialState :: FreestS
initialState = FreestS { runOpts    = initialOpts
                       , varEnv     = Map.empty
                       , prog       = Map.empty
                       , typeEnv    = Map.empty
                       , typenames  = Map.empty
                       , warnings   = []
                       , errors     = []
                       , nextIndex  = 0
                       , parseEnv   = Map.empty
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
  next <- gets nextIndex
  modify (\s -> s { nextIndex = next + 1 })
  return next

freshTVar :: String -> Pos -> FreestState TypeVar
freshTVar s p = mkVar p . (s ++) . show <$> getNextIndex

-- | FILE NAME

getFileName :: FreestState String
getFileName = fromMaybe "" . runFilePath <$> gets runOpts

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

addDualof :: T.Type -> FreestState ()
addDualof d@(T.Dualof p t) = do
  tn <- getTypeNames
  case tn Map.!? pos t of
    Just (T.Dualof _ _) -> return ()
    Just u -> modify (\s -> s { typenames = Map.insert p (T.Dualof p u) tn })
    Nothing -> modify (\s -> s { typenames = Map.insert p d tn })
addDualof t = internalError "Util.FreestState.addDualof" t

-- | WARNINGS

getWarnings :: FreestS -> String
getWarnings s =
   (intercalate "\n" . map f . take 10 . reverse . warnings) s
  where
    f = formatWarning (runFilePath $ runOpts s) (typenames s)
--    cmp x y = compare (pos x) (pos y)

hasWarnings :: FreestS -> Bool
hasWarnings = not . null . warnings

addWarning :: WarningType -> FreestState ()
addWarning w = modify (\s -> s { warnings = w : warnings s })

-- | ERRORS

getErrors :: PreludeNames -> FreestS -> String
getErrors v s =
   (intercalate "\n" . map f . take 10 . reverse . errors) s
--   (intercalate "\n" . map f . take 10 . sortBy errCmp . reverse . errors) s
  where
    f = formatError (runFilePath $ runOpts s) (typenames s) v
--    errCmp x y = compare (pos x) (pos y)

hasErrors :: FreestS -> Bool
hasErrors = not . null . errors

addError :: ErrorType -> FreestState ()
addError e = modify (\s -> s { errors = e : errors s })

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

-- | Run Options

data RunOpts = RunOpts { runFilePath  :: Maybe FilePath
--                     , preludeFile  :: Maybe FilePath
                       , mainFunction :: Maybe ProgVar
                       , quietmode    :: Bool
                       } deriving Show

instance Semigroup RunOpts where
  o1 <> o2 = RunOpts { runFilePath  = runFilePath o1 <|> runFilePath o2
--                    , preludeFile  = preludeFile o1 <|> preludeFile o2
                     , mainFunction = mainFunction o1 <|> mainFunction o2
                     , quietmode    = quietmode o1 || quietmode o2
                     }

defaultOpts :: RunOpts
defaultOpts = RunOpts { runFilePath  = Nothing
--                    , preludeFile  = Just "Prelude.fst"
                      , mainFunction = Nothing
                      , quietmode    = False
                      }

initialOpts :: RunOpts
initialOpts = RunOpts Nothing Nothing False -- Nothing

getMain :: RunOpts -> ProgVar
getMain opts = fromMaybe (mkVar defaultPos "main") maybeMain
  where maybeMain = mainFunction opts

isMainFlagSet :: RunOpts -> Bool
isMainFlagSet = isJust . mainFunction

isQuietFlagSet :: RunOpts -> Bool
isQuietFlagSet = quietmode

getOpts :: FreestState RunOpts
getOpts = gets runOpts
