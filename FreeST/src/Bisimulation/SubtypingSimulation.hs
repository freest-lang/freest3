{- |
Module      :  Bisimulation.SubtypingSimulation
Description :  Simulation-based subtyping, adapted from https://doi.org/10.4230/LIPIcs.CONCUR.2023.11
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  gsilva@lasige.di.fc.ul.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

module Bisimulation.SubtypingSimulation ( subtypeSimilar ) where


import qualified Data.Map as Map
import qualified Data.Sequence as Queue
import qualified Data.Set as Set
import Data.Tuple (swap)

import Bisimulation.Bisimulation
import Bisimulation.Grammar
import Bisimulation.Minimal ( minimal )
import Bisimulation.Norm (allNormed)
import Bisimulation.TypeToGrammar ( convertToGrammar )
import Equivalence.AlphaCongruence ()
import qualified Syntax.Type as T 


subtypeSimilar :: T.Type -> T.Type -> Bool
subtypeSimilar t u = subtypeGrammar (convertToGrammar [minimal t, minimal u])

-- | Assumes a grammar without unreachable symbols
subtypeGrammar :: Grammar -> Bool
subtypeGrammar g@(Grammar [xs, ys] ps) = expand expandPairSub queue rules ps
 where rules | allNormed ps = [reflex, headCongruence, bpa2]
             | otherwise    = [reflex, headCongruence, bpa1, bpa2]
       queue = Queue.singleton (Set.singleton (xs, ys), Set.empty)

-- XYZW-expansion at the level of pairs of words 
-- https://doi.org/10.4230/LIPIcs.CONCUR.2023.11, Definition 17.
expandPairSub :: PairExpander
expandPairSub ps (xs, ys) =
  -- extract transitions
  let ts1 = transitions xs ps
      ts2 = transitions ys ps in 
  -- expansion must hold on labels of all X, Y, Z and W sets
  Set.unions <$> sequence
    -- expand on X labels
    [ let ts1X = filterX ts1; ts2X = filterX ts2 in 
      if Map.keysSet ts1X `Set.isSubsetOf` Map.keysSet ts2X
        then Just (matchTrans ts1X ts2X)
        else Nothing
    -- expand on Y labels
    , let ts1Y = filterY ts1; ts2Y = filterY ts2 in 
      if Map.keysSet ts2Y `Set.isSubsetOf` Map.keysSet ts1Y
        then Just (matchTrans ts1Y ts2Y)
        else Nothing
    -- expand on Z labels
    , let ts1Z = filterZ ts1; ts2Z = filterZ ts2 in 
      if Map.keysSet ts1Z `Set.isSubsetOf` Map.keysSet ts2Z
        then Just (Set.map swap $ matchTrans ts1Z ts2Z)
        else Nothing
    -- expand on W labels
    , let ts1W = filterW ts1; ts2W = filterW ts2 in 
      if Map.keysSet ts2W `Set.isSubsetOf` Map.keysSet ts1W
        then Just (Set.map swap $ matchTrans ts1W ts2W)
        else Nothing
    ]
  where
    -- Membership in the X, Y, Z and W sets specifies the kind of
    -- simulation to be tested. This membership assignment specifies
    -- the subtyping simulation outlined in 
    -- https://doi.org/10.4230/LIPIcs.CONCUR.2023.11, Definition 8.
    memberX, memberY, memberZ, memberW :: Label -> Bool 
    -- X
    memberX (Label (T.Choice T.Internal) _)  = False 
    memberX (Label T.Record _)               = False 
    memberX ArrowD                           = False
    memberX (MessageP T.Out)                 = False
    memberX _                                = True
    -- Y
    memberY (Label (T.Choice T.External) _) = False 
    memberY (Label T.Variant _)             = False
    memberY ArrowD                           = False
    memberY Arrow1                           = False
    memberY (MessageP T.Out)                 = False 
    memberY _                                = True
    -- Z
    memberZ (MessageP T.Out)                 = True
    memberZ ArrowD                           = True
    memberZ _                                = False
    -- W
    memberW = memberZ

    -- Filter transitions according to the XYZW-membership of their labels
    filterX, filterY, filterZ, filterW :: Transitions -> Transitions
    [filterX, filterY, filterZ, filterW] = 
      map (\f -> Map.filterWithKey (\k _ -> f k)) 
          [memberX, memberY, memberZ, memberW]

    -- Match transitions with the same label
    matchTrans :: Transitions -> Transitions -> Node
    matchTrans m1 m2 = Set.fromList $ Map.elems $ Map.intersectionWith (,) m1 m2