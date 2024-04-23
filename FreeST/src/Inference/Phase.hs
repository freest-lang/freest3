{-# LANGUAGE TypeFamilies #-}
module Inference.Phase where

import           Syntax.AST
-- import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import Syntax.Constraint
import           Util.State
import           Syntax.Base

import qualified Data.Set as Set
import Control.Monad.State
-- import           Data.Void

data Inference

type InferenceS = FreestS Inference

type instance XDef Inference = E.Exp -- if it is in the end
type instance XExtra Inference = Extra

data Extra = Extra
  {
    -- List of variables (to build the initial sigma)
    -- Constraints
    mVariables :: Set.Set Variable
  , pkVariables :: Set.Set Variable
  , constraints :: [Constraint] -- Set? kind is not ord... 
  }


type InfState = FreestState Inference

addConstraint :: Constraint -> InfState ()
addConstraint c = 
  modify (\s -> s{extra = (extra s){constraints = c:constraints (extra s)}})

getConstraints :: InfState [Constraint]
getConstraints = gets (constraints . extra)

-- emptyConstraints :: InfState ()
-- emptyConstraints = modify (\s -> s{extra = (extra s){constraints = []}})

addMVariable :: Variable -> InfState ()
addMVariable x =
  modify (\s -> s{extra = (extra s){mVariables = Set.insert x (mVariables (extra s))}})

addPKVariable :: Variable -> InfState ()
addPKVariable x =
  modify (\s -> s{extra = (extra s){pkVariables = Set.insert x (pkVariables (extra s))}})

getMVariables, getPKVariables :: InfState (Set.Set Variable)
getMVariables = gets (mVariables . extra)
getPKVariables = gets (pkVariables . extra)

freshMultVar :: Span -> InfState Variable
freshMultVar s = do
  v <- mkVar s . ("φ" ++) . show <$> getNextIndex
  addMVariable v
  pure v

freshPKVar :: Span -> InfState Variable
freshPKVar s = do
  v <- mkVar s . ("ψ" ++) . show <$> getNextIndex
  addPKVariable v
  pure v

