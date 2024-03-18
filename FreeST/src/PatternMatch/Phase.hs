{-# LANGUAGE TypeFamilies #-}
module PatternMatch.Phase where

import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Util.State
import qualified Parse.Phase as PP

-- import           Data.Void
import           Control.Monad.State

data PatternMatch
data Elab

type instance XDef PatternMatch = ([Variable], E.Exp)
type instance XExtra PatternMatch = Extra

type PatternS = FreestS PatternMatch 
type PatternState = FreestState PatternMatch

type Defs = Definitions PatternMatch

-- extraPattern :: XExtra PatternMatch
-- extraPattern = void

data Extra = Extra
  { pEnvChoices :: PP.ChoicesLabels
  }

extraPattern :: Extra
extraPattern = Extra { pEnvChoices = [] }

getPEnvChoices :: PatternState PP.ChoicesLabels
getPEnvChoices = gets (pEnvChoices . extra)
