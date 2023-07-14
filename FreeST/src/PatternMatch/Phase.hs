{-# LANGUAGE TypeFamilies #-}
module PatternMatch.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State

import           Data.Void

data PatternMatch
data Elab

type instance XDef PatternMatch = ([Variable], E.Exp)
type instance XExtra PatternMatch = Void

type PatternS = FreestS PatternMatch 
type PatternState = FreestState PatternMatch

type Defs = Definitions PatternMatch

extraPattern :: XExtra PatternMatch
extraPattern = void

