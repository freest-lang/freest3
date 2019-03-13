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
( ConstructorEnv
, ExpEnv
, VarEnv
, KindEnv
) where 

import           Syntax.Position
import           Syntax.Kinds
import           Syntax.Types
import           Syntax.Exps
import qualified Data.Map.Strict as Map

-- The datatypes
type ConstructorEnv = Map.Map Bind (Pos, TypeScheme) -- TODO remove Pos

-- The signatures of the named functions in a program
type VarEnv = Map.Map Bind (Pos, TypeScheme) -- TODO remove Pos

-- The named functions in a program
type ExpEnv = Map.Map Bind (Pos, [Bind], Expression) -- TODO remove Pos

type KindEnv = Map.Map Bind (Pos, Kind) -- TODO remove Pos
