{-# LANGUAGE TypeFamilies #-}
module Elaboration.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State

import           Data.Void

data Elab

type instance XDef Elab = ([Variable], E.Exp)
type instance XExtra Elab = Void

type ElabS = FreestS Elab
type ElabState = FreestState Elab

type Defs = Definitions Elab

extraElab :: XExtra Elab
extraElab = void
