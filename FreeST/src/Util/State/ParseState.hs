{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Util.State.ParseState () where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error
-- import           Util.Warning

import           Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

type ModuleName = Maybe FilePath
type Imports = Set.Set FilePath 
type Errors = [ErrorType]
type ParseEnvChoices = [Variable]

data Parse

data ParseS = ParseS
  { ast :: AST Parse
  , moduleName :: ModuleName
  , imports :: Imports
  , errors :: Errors
  , parseEnvChoices :: ParseEnvChoices
  , nextIndex :: Int
  }

type ParseState = State ParseS

type instance XDef Parse = [([E.Pattern], E.Exp)]
-- type ParseDef = XDef Parse

setModuleName :: ModuleName -> ParseState ()
setModuleName moduleName = modify (\s -> s{moduleName})

addImport :: FilePath -> ParseState ()
addImport imp = modify (\s -> s{imports = imp `Set.insert` imports s})

-- addToPEnvChoices :: MonadState FreestS m => [Variable] -> m ()
addToPEnvChoices :: [Variable] -> ParseState ()
addToPEnvChoices xs =
  modify (\s -> s{ parseEnvChoices = parseEnvChoices s ++ xs })

addToSignatures :: Variable -> T.Type -> ParseState ()
addToSignatures x t = modify (\s -> s{ast = addSignature x t (ast s)})

addToTypes :: Variable -> K.Kind -> T.Type -> ParseState ()
addToTypes x k t = modify (\s -> s{ast = addType x k t (ast s)})

-- Specialised version without using addDefinition (from AST)
-- In this phase this function substitutes `addToPEnvPat`
addToDefinitions :: Variable -> [E.Pattern] -> E.Exp -> ParseState ()
addToDefinitions x xs e = modify (\s -> s{ast = (ast s){definitions = insert s}})
  where
    insert s = Map.insertWith add x [(xs,e)] (definitions (ast s))
    add b a = (++) a b
    
-- addError :: MonadState FreestS m => ErrorType -> m ()
addError :: ErrorType -> ParseState ()
addError e = modify (\s -> s { errors = e : errors s })


-- index is common

getNextIndex :: ParseState Int
getNextIndex = do
  next <- gets nextIndex
  modify (\s -> s { nextIndex = next + 1 })
  return next
  
freshVar :: String -> Span -> ParseState Variable
freshVar s p = mkVar p . (s ++) . show <$> getNextIndex

-- FreestState.hs:49:type ParseEnvPat = Map.Map Variable [([Pattern], Exp)]
-- FreestState.hs:48:type ParseEnv    = Map.Map Variable ([Variable], Exp)
-- FreestState.hs:51:type ParseEnvChoices = [Variable]
