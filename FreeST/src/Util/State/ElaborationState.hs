{-# LANGUAGE TypeFamilies #-}
module Util.State.ElaborationState() where

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

data Elab

data ElabS = ElabS
  { ast :: AST Elab
  , errors :: Errors
  , nextIndex :: Int
  }

type instance XDef Elab = E.Exp
