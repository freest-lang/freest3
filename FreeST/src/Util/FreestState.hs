{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Util.FreestState where

import           Syntax.Base
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Warning
-- import           Util.WarningMessage ()
-- import           Util.PrettyWarning ()
import           Util.Error
import           Util.Warning
-- import           Util.PrettyError ()

import           Control.Monad.State
import           Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import           Debug.Trace -- debug (used on debugM function)

-- | The typing state

type Warnings = [WarningType]

type Errors = [ErrorType]

type Imports = Set.Set FilePath 

type ParseEnv    = Map.Map Variable ([Variable], Exp)
type ParseEnvPat = Map.Map Variable [([Pattern], Exp)]

data FreestS = FreestS {
    runOpts         :: RunOpts
  , varEnv          :: VarEnv
  , prog            :: Prog
  , typeEnv         :: TypeEnv
  , typenames       :: TypeOpsEnv
  , warnings        :: Warnings
  , errors          :: Errors
  , nextIndex       :: Int
  , parseEnv        :: ParseEnv    -- "discarded" after elaboration
  , parseEnvPat     :: ParseEnvPat -- for pattern elimination
  , moduleName      :: Maybe FilePath
  , imports         :: Imports
  } -- deriving Show -- FOR DEBUG purposes

type FreestState = State FreestS

-- | Initial State

initialState :: FreestS
initialState = FreestS { runOpts         = defaultOpts
                       , varEnv          = Map.empty
                       , prog            = Map.empty
                       , typeEnv         = Map.empty
                       , typenames       = Map.empty
                       , warnings        = []
                       , errors          = []
                       , nextIndex       = 0
                       , parseEnv        = Map.empty
                       , parseEnvPat     = Map.empty
                       , moduleName      = Nothing
                       , imports         = Set.empty
                       }

-- | Parse Env

emptyPEnv :: FreestS -> FreestS
emptyPEnv s = s { parseEnv = Map.empty }

addToPEnv :: MonadState FreestS m => Variable -> [Variable] -> Exp -> m ()
addToPEnv x xs e =
  modify (\s -> s { parseEnv = Map.insert x (xs, e) (parseEnv s) })

getPEnv :: FreestState ParseEnv
getPEnv = gets parseEnv

setPEnv :: ParseEnv -> FreestState ()
setPEnv parseEnv = modify (\s -> s { parseEnv })

-- | Parse Env Pat (with Patterns)

addToPEnvPat :: MonadState FreestS m => Variable -> [Pattern] -> Exp -> m ()
addToPEnvPat x xs e =
  modify (\s -> s 
    { parseEnvPat = Map.insertWith add x [(xs, e)] (parseEnvPat s) })
    where add b a = (++) a b

getPEnvPat :: FreestState ParseEnvPat
getPEnvPat = gets parseEnvPat

setPEnvPat :: ParseEnvPat -> FreestState ()
setPEnvPat parseEnvPat =  modify (\s -> s { parseEnvPat })

-- | NEXT VAR

getNextIndex :: MonadState FreestS m => m Int
getNextIndex = do
  next <- gets nextIndex
  modify (\s -> s { nextIndex = next + 1 })
  return next

freshTVar :: MonadState FreestS m => String -> Span -> m Variable
freshTVar s p = mkVar p . (s ++) . show <$> getNextIndex

-- | VAR ENV

getVEnv :: FreestState VarEnv
getVEnv = gets varEnv

getFromVEnv :: Variable -> FreestState (Maybe T.Type)
getFromVEnv x = do
  vEnv <- getVEnv
  return $ vEnv Map.!? x

removeFromVEnv :: Variable -> FreestState ()
removeFromVEnv b = modify (\s -> s { varEnv = Map.delete b (varEnv s) })

addToVEnv :: MonadState FreestS m => Variable -> T.Type -> m ()
addToVEnv b t = modify (\s -> s { varEnv = Map.insert b t (varEnv s) })

-- vEnvMember :: Variable -> FreestState Bool
-- vEnvMember x = Map.member x <$> getVEnv

setVEnv :: VarEnv -> FreestState ()
setVEnv varEnv = modify (\s -> s { varEnv })

-- | EXP ENV

getProg :: FreestState Prog
getProg = gets prog

getFromProg :: Variable -> FreestState (Maybe Exp)
getFromProg x = do
  eEnv <- getProg
  return $ eEnv Map.!? x

addToProg :: Variable -> Exp -> FreestState ()
addToProg k v = modify (\s -> s { prog = Map.insert k v (prog s) })

setProg :: Prog -> FreestState ()
setProg prog = modify (\s -> s { prog })

-- | TYPE ENV

getTEnv :: FreestState TypeEnv
getTEnv = gets typeEnv

addToTEnv :: MonadState FreestS m => Variable -> Kind -> T.Type -> m ()
addToTEnv x k t =
  modify (\s -> s { typeEnv = Map.insert x (k, t) (typeEnv s) })

getFromTEnv :: Variable -> FreestState (Maybe (Kind, T.Type))
getFromTEnv b = do
  tEnv <- getTEnv
  return $ tEnv Map.!? b

setTEnv :: TypeEnv -> FreestState ()
setTEnv typeEnv = modify (\s -> s { typeEnv })

-- | TYPENAMES

addTypeName :: Span -> T.Type -> FreestState ()
addTypeName p t = modify (\s -> s { typenames = Map.insert p t (typenames s) })

getTypeNames :: FreestState TypeOpsEnv
getTypeNames = gets typenames

findTypeName :: Span -> T.Type -> FreestState T.Type
findTypeName p t = Map.findWithDefault t p <$> getTypeNames

addDualof :: T.Type -> FreestState ()
addDualof d@(T.Dualof p t) = do
  tn <- getTypeNames
  case tn Map.!? (getSpan t) of
    Just (T.Dualof _ _) -> return ()
    Just u -> modify (\s -> s { typenames = Map.insert p (T.Dualof p u) tn })
    Nothing -> modify (\s -> s { typenames = Map.insert p d tn })
addDualof t = internalError "Util.FreestState.addDualof" t

-- | WARNINGS

getWarnings :: FreestS -> String
getWarnings s =
   (intercalate "\n" . map f . take 10 . reverse . warnings) s
  where
    f = showWarnings (runFilePath $ runOpts s) (typenames s)

hasWarnings :: FreestS -> Bool
hasWarnings = not . null . warnings

addWarning :: WarningType -> FreestState ()
addWarning w = modify (\s -> s { warnings = w : warnings s })

-- | ERRORS

getErrors :: FreestS -> String
getErrors s = (intercalate "\n" . map f . take 10 . reverse . errors) s
  where f = showErrors (isStylable $ runOpts s) (runFilePath $ runOpts s) (typenames s)

hasErrors :: FreestS -> Bool
hasErrors = not . null . errors

-- addError :: ErrorType -> FreestState ()
addError :: MonadState FreestS m => ErrorType -> m ()
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

data RunOpts = RunOpts { runFilePath  :: FilePath
--                     , preludeFile  :: Maybe FilePath
                       , mainFunction :: Maybe Variable
                       , isStylable   :: Bool
                       , quietmode    :: Bool
                       } deriving Show


defaultOpts :: RunOpts
defaultOpts = RunOpts { runFilePath  = ""
--                    , preludeFile  = Just "Prelude.fst"
                      , mainFunction = Nothing
                      , isStylable   = True
                      , quietmode    = False
                      }

getMain :: RunOpts -> Variable
getMain opts = fromMaybe (mkVar defaultSpan "main") maybeMain
  where maybeMain = mainFunction opts

isMainFlagSet :: RunOpts -> Bool
isMainFlagSet = isJust . mainFunction

getOpts :: MonadState FreestS m => m RunOpts
getOpts = gets runOpts

-- | FILE NAME

getFileName :: MonadState FreestS m => m String
getFileName = runFilePath <$> getOpts


-- | Modules & Imports

setModuleName :: MonadState FreestS m => Maybe FilePath -> m ()
setModuleName moduleName = modify (\s -> s { moduleName })

getModuleName :: MonadState FreestS m => m (Maybe FilePath)
getModuleName = gets moduleName

setImports :: MonadState FreestS m => Imports -> m ()
setImports imports = modify (\s -> s { imports })

addImport :: MonadState FreestS m => FilePath -> m ()
addImport imp = modify (\s -> s { imports = Set.insert imp (imports s) })

getImports :: MonadState FreestS m => m Imports
getImports = gets imports 



