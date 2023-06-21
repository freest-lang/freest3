{-# LANGUAGE TypeFamilies #-}
module Elaboration.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State.State

import           Data.Void

data PatternMatch
data Elaboration

type instance XDef PatternMatch = ([Variable], E.Exp)
type instance XExtra PatternMatch = Void

type instance XDef Elaboration = E.Exp
type instance XExtra Elaboration = Void
