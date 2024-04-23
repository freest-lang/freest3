{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Elaboration.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State
import Syntax.Kind

--import           Data.Void
import qualified Data.Set as Set
import Control.Monad.State 

data Elab

type instance XDef Elab = ([Variable], E.Exp)
type instance XExtra Elab = Extra

data Extra = Extra
  { mVariables  :: Set.Set Variable
  , pkVariables  :: Set.Set Variable
  }


type ElabS = FreestS Elab
type ElabState = FreestState Elab

type Defs = Definitions Elab

extraElab :: XExtra Elab
extraElab = Extra { .. }
  where
    mVariables  = Set.empty
    pkVariables  = Set.empty
  

initialElab :: FreestS Elab
initialElab = initial Extra { .. } -- initial void
  where
    mVariables  = Set.empty
    pkVariables  = Set.empty

freshKVar :: Span -> ElabState Kind
freshKVar s = do
  mv <- mkVar s . ("φ" ++) . show <$> getNextIndex
  pk <- mkVar s . ("ψ" ++) . show <$> getNextIndex
  modify (\s -> s{extra = (extra s) {pkVariables = Set.insert pk (pkVariables $ extra s)}})
  modify (\s -> s{extra = (extra s) {mVariables = Set.insert mv (mVariables $ extra s)}})
  return $ Kind s (MultVar mv) (PKVar pk)
    
