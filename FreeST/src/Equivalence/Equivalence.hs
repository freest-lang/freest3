{- |
Module      :  Equivalence.Equivalence
Description :  Type equivalence
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Try first alpha equivalence; if it fails try bisimulation
-}

module Equivalence.Equivalence
  ( equivalent
  )
where

import Equivalence.AlphaEquivalence
import Bisimulation.Bisimulation
import qualified Syntax.Type                   as T

equivalent :: T.Type -> T.Type -> Bool
equivalent t u =
  t == u || -- Alpha-equivalence, 30% speed up in :program tests
  bisimilar t u
