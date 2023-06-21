{-# LANGUAGE TypeFamilies #-}
module Validation.Phase where

import           Syntax.AST
import qualified Syntax.Expression as E
import           Util.State.State

import           Data.Void

data Typing

type instance XDef Typing = E.Exp -- Prog
type instance XExtra Typing = Void

type TypingState = FreestState Typing
