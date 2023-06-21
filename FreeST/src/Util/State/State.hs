{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Util.State.State where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.Warning

import           Control.Monad.State
import           Data.List ( intercalate )
import qualified Data.Map.Strict as Map

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
type FreestState a = State (FreestS a)

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

-- | AST

getAST :: FreestState a (AST a)
getAST = gets ast

addToSignatures :: MonadState (FreestS a) m => Variable -> T.Type -> m ()
addToSignatures b t = modify (\s -> s{ast = addSignature b t (ast s)})

getSignatures ::  MonadState (FreestS a) m => m Signatures
getSignatures = gets (signatures . ast)

getSignaturesS ::  FreestS a -> Signatures
getSignaturesS = signatures . ast

getFromSignatures ::  MonadState (FreestS a) m => Variable -> m (Maybe T.Type)
getFromSignatures = (`fmap` getSignatures) . flip (Map.!?)  

getTypes :: MonadState (FreestS a) m => m Types
getTypes =  gets (types . ast)

getTypesS :: FreestS a -> Types
getTypesS =  types . ast

addToDefinitions :: MonadState (FreestS a) m => Variable -> XDef a -> m ()
addToDefinitions x t = modify (\s -> s{ast = addDefinition x t (ast s)})

getDefs :: MonadState (FreestS a) m => m (Definitions a)
getDefs =  gets (definitions . ast)

getDefsS :: FreestS a -> Definitions a
getDefsS =  definitions . ast



addToTypes :: MonadState (FreestS a) m => Variable -> K.Kind -> T.Type -> m ()
addToTypes x k t = modify (\s -> s{ast = addType x k t (ast s)})

-- | INDEX

getNextIndex :: MonadState (FreestS a) m => m Int
getNextIndex = do
  i <- gets nextIndex
  modify (\s -> s{nextIndex = i + 1})
  return i

-- | ERRORS

getErrors :: RunOpts -> FreestS a -> String
getErrors runOpts s = (intercalate "\n" . map f . take 10 . reverse . errors) s
  where f = showErrors (isStylable runOpts) (runFilePath runOpts) (typenames s)

hasErrors :: FreestS a -> Bool
hasErrors = not . null . errors

addError :: MonadState (FreestS a) m => ErrorType -> m ()
addError e = modify (\s -> s { errors = e : errors s })
  
-- | WARNINGS

getWarnings :: RunOpts -> FreestS a -> String
getWarnings runOpts s = (intercalate "\n" . map f . take 10 . reverse . warnings) s
  where f = showWarnings (runFilePath runOpts) (typenames s)

hasWarnings :: FreestS a -> Bool
hasWarnings = not . null . warnings

addWarning :: WarningType -> FreestState a ()
addWarning w = modify (\s -> s { warnings = w : warnings s })

-- | Fresh var
freshTVar :: MonadState (FreestS a) m => String -> Span -> m Variable
freshTVar s p = mkVar p . (s ++) . show <$> getNextIndex


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

-- | OTHER MODULE

typeListToRcdType :: [(Variable, [T.Type])] -> T.TypeMap
typeListToRcdType []             = Map.empty
typeListToRcdType ((c, us) : ts) =
  Map.insert c (T.Labelled (getSpan c) T.Record $ typesToMap 0 us) (typeListToRcdType ts)
  where typesToMap n [] = Map.empty
        typesToMap n (t : ts) = Map.insert (mkVar (getSpan t) $ show n) t (typesToMap (n+1) ts)
