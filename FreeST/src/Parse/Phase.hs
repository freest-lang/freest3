{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Parse.Phase where

import           Syntax.AST
import           Syntax.Base hiding (moduleName)
import qualified Syntax.Expression as E
import           Util.Error
import           Util.State

import           Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

type Imports = Set.Set FilePath
type ModuleName = Maybe FilePath
type ChoicesLabels = [Variable]

data Parse

type instance XDef Parse = [([E.Pattern], E.Exp)] -- ~ ParseEnvPat
type instance XExtra Parse = Extra 

type ParseS = FreestS Parse
type ParseState = StateT ParseS (Either ErrorType)
type Defs = Definitions Parse
  
data Extra = Extra
  { moduleName  :: ModuleName
  , imports     :: Imports
  , pEnvChoices :: ChoicesLabels
  , runOpts     :: RunOpts
  }

initialExtraParse :: Extra
initialExtraParse = Extra { .. }
  where
    moduleName  = Nothing
    imports     = Set.empty
    pEnvChoices = []
    runOpts     = defaultOpts

-- | State with file name

initialWithFile :: FilePath -> FreestS Parse 
initialWithFile runFilePath = initial Extra{..}
  where
    moduleName  = Nothing
    imports     = Set.empty
    pEnvChoices = []
    runOpts     = defaultOpts{runFilePath}

setModuleName :: ModuleName -> ParseState ()
setModuleName moduleName = modify (\s -> s{extra = (extra s){moduleName}})

setModule :: FreestS Parse -> ModuleName -> FreestS Parse
setModule s moduleName = s{extra = (extra s){moduleName}}

getModule :: FreestS Parse -> ModuleName
getModule = moduleName . extra

getModuleName :: ParseState ModuleName
getModuleName = gets (moduleName . extra)

addImport :: FilePath -> ParseState ()
addImport imp = modify (\s -> s{extra = (extra s){imports = Set.insert imp (imports $ extra s)}})
-- addImport imp = modify (\s -> s{extra = second (Set.insert imp) (extra s)})

getImports :: ParseState Imports
getImports = gets (imports . extra)

getImps :: FreestS Parse -> Imports
getImps = imports . extra

addToPEnvChoices :: [Variable] -> ParseState ()
addToPEnvChoices vs = modify (\s -> s{extra = (extra s){pEnvChoices = pEnvChoices (extra s) ++ vs}})

addToPEnvPat :: Variable -> [E.Pattern] -> E.Exp -> ParseState ()
addToPEnvPat x xs e =
  modify (\s -> s { ast = (ast s)
   { definitions = Map.insertWith add x [(xs, e)] (definitions $ ast s)} })
    where add b a = (++) a b

-- | FILE NAME

getFileName :: ParseState FilePath
getFileName = gets (runFilePath . runOpts . extra)

getFName :: FreestS Parse -> FilePath
getFName = runFilePath . runOpts . extra

setFName :: FilePath -> FreestS Parse -> FreestS Parse
setFName runFilePath s = s{extra = (extra s){runOpts = (runOpts $ extra s){runFilePath}}}

-- data Extra = Extra
--   { moduleName  :: ModuleName
--   , imports     :: Imports
--   , pEnvChoices :: ParseEnvChoices
--   , runOpts     :: RunOpts
--   }
