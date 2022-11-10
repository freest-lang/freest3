{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE FlexibleInstances #-}

module Equivalence.Subtyping
 ( Subsort(..)
 )
 where

import           Bisimulation.Bisimulation ( subsimilar )
import           Validation.Subkind ( (<:), Subsort )
import qualified Syntax.Type as T

instance Subsort T.Type where 
    (<:) = subsimilar
