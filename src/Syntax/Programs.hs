{- |
Module      :  Programs
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Programs
( KindEnv
, ConstructorEnv
, ExpEnv
, VarEnv
) where 

import           Syntax.Position
import           Syntax.Kinds
import           Syntax.Types
import           Syntax.Exps
import qualified Data.Map.Strict as Map

type ConstructorEnv = Map.Map TypeVar (Pos, TypeScheme) -- TODO remove Pos

-- The named functions in a program
type ExpEnv = Map.Map TermVar (Pos, Params, Expression) -- TODO remove Pos

type KindVar = String -- TODO: move to Kind

type KindEnv = Map.Map KindVar (Pos, Kind) -- TODO remove Pos, move to Typing

type VarEnv = Map.Map TermVar (Pos, TypeScheme) -- TODO remove Pos, move to Typing

