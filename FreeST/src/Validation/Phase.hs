{-# LANGUAGE TypeFamilies #-}
module Validation.Phase where

import           Syntax.AST
import qualified Syntax.Expression as E
import           Util.State

import           Data.Void

data Typing

type instance XDef Typing = E.Exp -- Prog
type instance XExtra Typing = Void

type TypingS = FreestS Typing
type TypingState = FreestState Typing

type Defs = Definitions Typing -- (XDef Typing)

initialTyp :: FreestS Typing
initialTyp = initial void
