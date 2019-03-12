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
  , VarEnv
  , ExpEnv
--  , TypeEnv
  , ConstructorEnv
  )where 

import qualified Data.Map.Strict as Map
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds


type KindEnv = Map.Map TypeVar (Pos, Kind)

type VarEnv = Map.Map TermVar (Pos, TypeScheme)

type ExpEnv = Map.Map TermVar (Pos, Params, Expression)

-- type TypeEnv = Map.Map TypeVar Type

type ConstructorEnv = Map.Map TypeVar (Pos, TypeScheme)
