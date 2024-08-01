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
  , bisimilar -- for testing purposes
  )
where

import qualified Syntax.Type                 as T
import           Equivalence.AlphaCongruence
import           SimpleGrammar.TypeToGrammar ( convertToGrammar )
-- import qualified BisimulationTACAS.Bisimulation   as G ( bisimilar )
import qualified Bisimulation.Bisimulation   as G ( bisimilar )

equivalent :: T.Type -> T.Type -> Bool
equivalent t u = t == u || bisimilar t u

bisimilar :: T.Type -> T.Type -> Bool
bisimilar t u = G.bisimilar (convertToGrammar [t, u])
