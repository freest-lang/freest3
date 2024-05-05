{- |
Module      :  Equivalence.Subtyping
Description :  Type equivalence
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  gsilva@lasige.di.fc.ul.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Try first alpha equivalence; if it fails try subtyping.
-}

module Equivalence.Subtyping
  ( subtype
  )
where

import Equivalence.AlphaCongruence()
import Bisimulation.SubtypingSimulation (subtypeSimilar)
import qualified Syntax.Type                   as T

subtype :: T.Type -> T.Type -> Bool
subtype t u =
  t == u || -- Alpha-congruence, 30% speed up in :program tests
  subtypeSimilar t u
