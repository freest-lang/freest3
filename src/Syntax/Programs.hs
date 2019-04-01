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
import           Syntax.Kinds
import           Syntax.Types
import           Syntax.Expression
import qualified Data.Map.Strict as Map

-- The type definitions for Named datatypes or type
type TypeEnv = Map.Map TBind (Kind, TypeScheme)

-- The signatures of the functions and other variables in a Program
type VarEnv = Map.Map PBind TypeScheme

-- The definitions of the named functions in a program
type ExpEnv = Map.Map PBind ([PBind], Expression)
