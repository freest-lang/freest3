{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Typing.Phase where

import           Syntax.AST
import qualified Syntax.Expression as E
import           Util.State
import Control.Monad.State

-- import           Data.Void

data Typing

type instance XDef Typing = E.Exp -- Prog
type instance XExtra Typing = RunOpts

type TypingS = FreestS Typing
type TypingState = FreestState Typing

type Defs = Definitions Typing -- (XDef Typing)

initialTyp :: RunOpts -> FreestS Typing
initialTyp = initial

getRunOpts :: TypingState RunOpts
getRunOpts = gets extra
