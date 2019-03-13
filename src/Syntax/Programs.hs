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

type KindVar = String -- TODO: move elsewhere

type KindEnv = Map.Map KindVar (Pos, Kind)

type ConstructorEnv = Map.Map TypeVar (Pos, TypeScheme)

type ExpEnv = Map.Map TermVar (Pos, Params, Expression)

type VarEnv = Map.Map TermVar (Pos, TypeScheme)

