{-# LANGUAGE TypeFamilies #-}
module Elaboration.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State

import           Data.Void

data PatternMatch
data Elab

type instance XDef PatternMatch = ([Variable], E.Exp)
type instance XExtra PatternMatch = Void

--type instance XDef Elab = E.Exp
type instance XDef Elab = ([Variable], E.Exp)
type instance XExtra Elab = Void

type FreestPattern = FreestS PatternMatch 
type FreestElab = FreestS Elab

type PatternState = FreestState PatternMatch
type ElabState = FreestState Elab

-- initExtraPMatch :: Extra 
type ParseEnv = Definitions PatternMatch


extraPattern :: XExtra PatternMatch
extraPattern = void

extraElab :: XExtra Elab
extraElab = void
