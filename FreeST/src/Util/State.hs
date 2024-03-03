{-# LANGUAGE NamedFieldPuns, FlexibleContexts, TypeFamilies #-}
module Util.State where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Kind as K
import           Syntax.Program
import           Syntax.MkName
import qualified Syntax.Type as T
import           Util.Error
import           Util.Warning

import qualified Control.Monad.State as S
import           Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Traversable as Traversable
import           Data.Void
import           Debug.Trace

type Warnings = [WarningType]
type Errors = [ErrorType]

data FreestS a = FreestS
  { ast :: AST a
  , nextIndex :: Int
  , errors :: Errors
  , warnings :: Warnings
  , typenames :: TypeOpsEnv -- TODO: Remove with the new errors 
  , extra :: XExtra a
  }

type family XExtra a
type FreestState a = S.State (FreestS a)

void :: Void
void = error "Attempt to evaluate void"

-- | Initial state

initial :: XExtra a -> FreestS a
initial ext = FreestS {
    ast = initialAST
  , nextIndex = 0
  , errors = []
  , warnings = []
  , typenames = Map.empty
  , extra = ext
  }

-- Dummy phase. This instance allows calling functions from a generic context
data Gen
type instance XExtra Gen  = Void

initialS :: FreestS Gen
initialS = FreestS {
    ast = initialAST
  , nextIndex = 0
  , errors = []
  , warnings = []
  , typenames = Map.empty
  , extra = void
  }

-- | AST

getAST :: FreestState a (AST a)
getAST = S.gets ast

-- | SIGNATURES

addToSignatures :: S.MonadState (FreestS a) m => Variable -> T.Type -> m ()
addToSignatures b t = S.modify (\s -> s{ast = addSignature b t (ast s)})

getSignatures ::  S.MonadState (FreestS a) m => m Signatures
getSignatures = S.gets (signatures . ast)

getSignaturesS ::  FreestS a -> Signatures
getSignaturesS = signatures . ast

getFromSignatures ::  S.MonadState (FreestS a) m => Variable -> m (Maybe T.Type)
getFromSignatures = (`fmap` getSignatures) . flip (Map.!?)  

setSignatures :: S.MonadState (FreestS a) m => Signatures -> m ()
setSignatures sigs = S.modify (\s -> s{ast = setSigs sigs (ast s)})

removeFromSignatures :: S.MonadState (FreestS a) m => Variable -> m ()
removeFromSignatures x = S.modify (\s -> s{ast = removeSig x (ast s)})

-- | TYPES

getTypes :: S.MonadState (FreestS a) m => m Types
getTypes =  S.gets (types . ast)

setTypes :: S.MonadState (FreestS a) m => Types -> m ()
setTypes types = S.modify(\s -> s{ast = setASTTypes types (ast s)})

getTypesS :: FreestS a -> Types
getTypesS =  types . ast

getFromTypes ::  S.MonadState (FreestS a) m => Variable -> m (Maybe (K.Kind, T.Type))
getFromTypes = (`fmap` getTypes) . flip (Map.!?)


addToDefinitions :: S.MonadState (FreestS a) m => Variable -> XDef a -> m ()
addToDefinitions x t = S.modify (\s -> s{ast = addDefinition x t (ast s)})

getFromDefinitions ::  S.MonadState (FreestS a) m => Variable -> m (Maybe (XDef a))
getFromDefinitions = (`fmap` getDefs) . flip (Map.!?)

getDefs :: S.MonadState (FreestS a) m => m (Definitions a)
getDefs =  S.gets (definitions . ast)

setDefs :: S.MonadState (FreestS a) m => Definitions a -> m ()
setDefs defs = S.modify (\s -> s{ast = setDefinitions defs (ast s)})

getDefsS :: FreestS a -> Definitions a
getDefsS =  definitions . ast

addToTypes :: S.MonadState (FreestS a) m => Variable -> K.Kind -> T.Type -> m ()
addToTypes x k t = S.modify (\s -> s{ast = addType x k t (ast s)})

-- | INDEX

getNextIndex :: S.MonadState (FreestS a) m => m Int
getNextIndex = do
  i <- S.gets nextIndex
  S.modify (\s -> s{nextIndex = i + 1})
  return i

-- | ERRORS

getErrors :: RunOpts -> FreestS a -> String
getErrors runOpts s = (intercalate "\n" . map f . take 10 . reverse . errors) s
  where f = showError (isStylable runOpts) (Left $ runFilePath runOpts) (typenames s)

hasErrors :: FreestS a -> Bool
hasErrors = not . null . errors

addError :: S.MonadState (FreestS a) m => ErrorType -> m ()
addError e = S.modify (\s -> s { errors = e : errors s })
  
setErrors :: S.MonadState (FreestS a) m => Errors -> m ()
setErrors errors = S.modify (\s -> s { errors })
  
-- | WARNINGS

getWarnings :: RunOpts -> FreestS a -> String
getWarnings runOpts s = (intercalate "\n" . map f . take 10 . reverse . warnings) s
  where f = showWarnings (isStylable runOpts) (runFilePath runOpts) (typenames s)

hasWarnings :: FreestS a -> Bool
hasWarnings = not . null . warnings

addWarning :: S.MonadState (FreestS a) m => WarningType -> m ()
addWarning w = S.modify (\s -> s { warnings = w : warnings s })


-- | RUNOPTS, Move to other module ???

data RunOpts = RunOpts { runFilePath  :: FilePath
--                     , preludeFile  :: Maybe FilePath
                       , args         :: [String]
                       , mainFunction :: Maybe Variable
                       , isStylable   :: Bool
                       , quietmode    :: Bool
                       } deriving Show

defaultOpts :: RunOpts
defaultOpts = RunOpts { runFilePath  = ""
--                    , preludeFile  = Just "Prelude.fst"
                      , args = []
                      , mainFunction = Nothing
                      , isStylable   = True
                      , quietmode    = False
                      }


isMainFlagSet :: RunOpts -> Bool
isMainFlagSet = isJust . mainFunction

getMain :: RunOpts -> Variable
getMain opts = fromMaybe mkMain maybeMain
  where maybeMain = mainFunction opts


-- | OTHER MODULE?

-- typeListToRcdType :: [(Variable, [T.Type])] -> T.TypeMap
-- typeListToRcdType []             = Map.empty
-- typeListToRcdType ((c, us) : ts) =
--   Map.insert c (T.Labelled (getSpan c) T.Record $ typesToMap 0 us) (typeListToRcdType ts)
--   where typesToMap n [] = Map.empty
--         typesToMap n (t : ts) = Map.insert (mkVar (getSpan t) $ show n) t (typesToMap (n+1) ts)


-- | Traversing Map.map over FreestStates

tMapM :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapM f m = Traversable.sequence (Map.map f m)

tMapM_ :: Monad m => (a1 -> m a2) -> Map.Map k a1 -> m ()
tMapM_ f m = S.void $ tMapM f m

tMapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
tMapWithKeyM f m = Traversable.sequence (Map.mapWithKey f m)

tMapWithKeyM_ :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m ()
tMapWithKeyM_ f m = S.void $ tMapWithKeyM f m

-- | TYPENAMES

addTypeName :: S.MonadState (FreestS a) m => Span -> T.Type -> m ()
addTypeName p t = S.modify (\s -> s { typenames = Map.insert p t (typenames s) })

getTypeNames :: S.MonadState (FreestS a) m => m TypeOpsEnv
getTypeNames = S.gets typenames

findTypeName :: S.MonadState (FreestS a) m => Span -> T.Type -> m T.Type
findTypeName p t = Map.findWithDefault t p <$> getTypeNames

addDualof :: S.MonadState (FreestS a) m => T.Type -> m ()
addDualof d@(T.Dualof p t) = do
  tn <- getTypeNames
  case tn Map.!? getSpan t of
    Just (T.Dualof _ _) -> return ()
    Just u -> S.modify (\s -> s { typenames = Map.insert p (T.Dualof p u) tn })
    Nothing -> S.modify (\s -> s { typenames = Map.insert p d tn })
addDualof t = internalError "Util.State.addDualof" t

-- | Debug Function

debugM :: S.MonadState (FreestS a) m => String -> m ()
debugM err = do
  i <- getNextIndex
  traceM $ "\n" ++ show i ++ ". " ++ err ++ "\n"
