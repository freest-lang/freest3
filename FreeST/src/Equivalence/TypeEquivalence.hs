{- |
Module      :  Equivalence.TypeEquivalence
Description :  Type equivalence
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Try first alpha equivalence; if it fails try bisimulation
-}

module Equivalence.TypeEquivalence
  ( equivalent
  )
where

import           Equivalence.AlphaCongruence()
import           Bisimulation.Bisimulation
import qualified Syntax.Type                   as T
import           Debug.Trace (trace)

equivalent :: T.Type -> T.Type -> Bool
equivalent t u =
  -- trace (show t ++ " ==\n" ++ show u ++ " = " ++ show (t == u))
  t == u || -- Alpha-congruence, 30% speed up in :program tests
  bisimilar t u
