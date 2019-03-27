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
( TypeEnv
, ExpEnv
, VarEnv
) where 

import           Syntax.Bind
import           Syntax.Types
import           Syntax.Exps
import qualified Data.Map.Strict as Map

-- The type definitions or abbreviations (data, type)
type TypeEnv = Map.Map KBind TypeScheme

-- The signatures of the named functions in a program
type VarEnv = Map.Map Bind TypeScheme

-- The definitions of the named functions in a program
type ExpEnv = Map.Map Bind ([Bind], Expression)
