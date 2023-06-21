{-# LANGUAGE TypeFamilies #-}
module Util.State.PatternMatchingState() where 

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error

import           Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Duplicated
type Errors = [ErrorType]

-- Parse -> PatternMatchingState -> ElaborationState

type ParseEnvChoices = [Variable]
data PMatch

data PMatchS = PMatchS
  { ast :: AST PMatch
  -- , moduleName :: ModuleName
  -- , imports :: Imports
  , errors :: Errors
  , parseEnvChoices :: ParseEnvChoices
  , nextIndex :: Int
  }

type PatternMatchState = State PMatchS

type instance XDef PMatch = ([Variable], E.Exp)

-- matchFuns :: ParseEnvPat -> ParseEnv
-- buildProg :: ParseEnv -> Prog

