{- |
Module      :  Equivalence.Bisimulation
Description :  A bisimulation
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a bisimulation. Function bisimilar first converts two context-free
session types into a grammar, which is pruned. An expansion tree is computed afterwards,
through an alternation of expansion of children nodes and their simplification, using the
reflexive, congruence, and BPA rules.
-}
{-# LANGUAGE TupleSections, LambdaCase, MultiWayIf #-}

module Bisimulation.Bisimulation
  ( bisimilar
  , bisimilarGrm -- For SGBisim
  , (<~)
  )
where

import           Syntax.Base                    (Variable, defaultSpan, Multiplicity (..)) -- Nonterminal symbols are type variables
import qualified Syntax.Type                   as T
import           Equivalence.TypeToGrammar      ( convertToGrammar )
import           Bisimulation.Grammar
import           Bisimulation.Norm
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Queue
import           Data.Bifunctor
import           Data.List                      ( isPrefixOf
                                                , union
                                                , isSuffixOf
                                                )
-- Word is (re)defined in module Equivalence.Grammar
import           Prelude                 hiding ( Word )
import           Debug.Trace
import Data.Function
import Control.Monad.Extra ((||^))
import Parse.Read

bisimilar :: T.Type -> T.Type ->  Bool
bisimilar t u = (t <~ u) && (u <~ t)

(<~) :: T.Type -> T.Type ->  Bool
t <~ u = bisimilarGrm $ convertToGrammar [t, u]

-- | Assumes a grammar without unreachable symbols
bisimilarGrm :: Grammar -> Bool
bisimilarGrm (Grammar [xs, ys] ps) = expand queue rules ps
 where
  rules | allNormed ps = [reflex, congruence, bpa2{-, filtering-}]
        | otherwise    = [reflex, congruence, bpa1, bpa2 {-, filtering-}]
  queue = Queue.singleton (Set.singleton (xs, ys), Set.empty)

type Node = Set.Set (Word, Word)

type Ancestors = Node

type Branch = (Node, Ancestors)

type BranchQueue = Queue.Seq Branch

type NodeTransformation = Productions -> Ancestors -> Node -> Set.Set Node

-- The expand-simplify loop

expand :: BranchQueue -> [NodeTransformation] -> Productions -> Bool
expand Queue.Empty _ _ = False
expand ((n, a) Queue.:<| q) rules ps
  | Set.null n = True
  | otherwise = case expandNode ps n of
    Nothing -> expand q rules ps
    Just n' -> expand (simplify q branch rules ps) rules ps
      where
        n'' = pruneNode ps n'
        branch = Set.singleton (n'', Set.union a n)

simplify
  :: BranchQueue
  -> Set.Set Branch
  -> [NodeTransformation]
  -> Productions
  -> BranchQueue
simplify q n rules ps = foldr enqueueBranch q (findFixedPoint n rules ps)

-- Enqueue at one end of the queue
enqueueBranch :: Branch -> BranchQueue -> BranchQueue
enqueueBranch (n, a) q | maxLength n <= 1 = (n, a) Queue.<| q
                       | otherwise        = q Queue.|> (n, a)

-- The maximum length of the pairs in a node
maxLength :: Node -> Int
maxLength n
  | Set.null n = 0
  | otherwise  = maximum (Set.map (\(a, b) -> max (length a) (length b)) n)

expandNode :: Productions -> Node -> Maybe Node
expandNode ps = Set.foldr
  (\p acc -> case acc of
    Nothing -> Nothing
    Just n' -> case expandPair ps p of
      Nothing  -> Nothing
      Just n'' -> Just (Set.union n' n'')
  )
  (Just Set.empty)
  
expandPair :: Productions -> (Word, Word) -> Maybe Node
expandPair ps (xs, ys) =
  if | (isChoicePair || isCoBranchPair) && not (Set.null ls2) && ls2 `Set.isSubsetOf` ls1
     -> Just $ match False m1 m2 Set.empty
     | (isBranchPair || isCoChoicePair) && not (Set.null ls1) && ls1 `Set.isSubsetOf` ls2
     -> Just $ match True m1 m2 Set.empty
     | isArrowPair || isCoArrowPair 
     -> let m1'  = if isArrowPair   && isLinArrowSet ls2 then toLinArrowTrans m1 else m1
            m2'  = if isCoArrowPair && isLinArrowSet ls1 then toLinArrowTrans m2 else m2
            (ls1', ls2') = (Map.keysSet m1', Map.keysSet m2') in 
        if ls1' == ls2'
          then Just $ match True (rTrans m1') (rTrans m2') (match False (dTrans m1') (dTrans m2') Set.empty)
          else Nothing
     | ls1 == ls2 -> Just $ match True m1 m2 Set.empty
     | otherwise  -> Nothing
 where
  (m1, m2)        = (transitions xs ps, transitions ys ps)
  (ls1, ls2)      = (Map.keysSet m1   , Map.keysSet m2   )
  allLabels       = ls1 `Set.union` ls2
  -- string manipulation is cumbersome and prone to errors, maybe labels should have their own datatype?
  isChoicePair    = all (isPrefixOf "+" ) allLabels
  isBranchPair    = all (isPrefixOf "&" ) allLabels
  isCoChoicePair  = all (isPrefixOf "^+") allLabels
  isCoBranchPair  = all (isPrefixOf "^&") allLabels
  isArrowPair     = all (isPrefixOf "->"  ||^ isPrefixOf "1->" ) allLabels
  isCoArrowPair   = all (isPrefixOf "^->" ||^ isPrefixOf "^1->") allLabels
  isLinArrowSet   = all (isPrefixOf "1->" ||^ isPrefixOf "^1->")
  toLinArrowTrans = Map.mapKeys (\case ('-':'>':cs) -> "1->"++cs
                                       ('^':'-':'>':cs) -> "^1->"++cs
                                       cs -> cs)
  dTrans          = Map.filterWithKey (\l xs -> "d" `isSuffixOf` l)
  rTrans          = Map.filterWithKey (\l xs -> "r" `isSuffixOf` l)

  match :: Bool -> Transitions -> Transitions -> Node -> Node
  match b m1 m2 n =
    if b then
      Map.foldrWithKey (\l xs n' -> Set.insert (xs, m2 Map.! l) n') n m1
    else
      Map.foldrWithKey (\l xs n' -> Set.insert (m1 Map.! l, xs) n') n m2

-- The fixed point of branch wrt the application of node transformations
findFixedPoint
  :: Set.Set Branch -> [NodeTransformation] -> Productions -> Set.Set Branch
findFixedPoint branch rules ps | branch == branch' = branch
                               | otherwise = findFixedPoint branch' rules ps
 where
  branch' = foldr apply branch rules
  apply :: NodeTransformation -> Set.Set Branch -> Set.Set Branch
  apply trans =
    foldr (\(n, a) bs -> Set.union (Set.map (, a) (trans ps a n)) bs) Set.empty

-- The various node transformations

reflex :: NodeTransformation
reflex _ _ = Set.singleton . Set.filter (uncurry (/=))

congruence :: NodeTransformation
congruence _ a = Set.singleton . Set.filter (not . congruentToAncestors)
 where
  congruentToAncestors :: (Word, Word) -> Bool
  congruentToAncestors p = or $ Set.map (congruentToPair p) a

  congruentToPair :: (Word, Word) -> (Word, Word) -> Bool
  congruentToPair (xs, ys) (xs', ys') =
    not (null xs')
      &&           xs'
      `isPrefixOf` xs
      &&           not (null ys')
      &&           ys'
      `isPrefixOf` ys
      &&           (xs'' == ys'' || congruentToAncestors (xs'', ys''))
   where
    xs'' = drop (length xs') xs
    ys'' = drop (length ys') ys

filtering :: NodeTransformation
filtering ps _ n | normsMatch = Set.singleton n
                 | otherwise  = Set.empty
  where normsMatch = and $ Set.map (uncurry (equallyNormed ps)) n

applyBpa
  :: (Productions -> Ancestors -> (Word, Word) -> Set.Set Node)
  -> NodeTransformation
applyBpa transf g a n = Set.foldr
  (\p ps ->
    Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (transf g a p)) ps
  )
  (Set.singleton n)
  n

bpa1 :: NodeTransformation
bpa1 = applyBpa bpa1'

bpa1' :: Productions -> Ancestors -> (Word, Word) -> Set.Set Node
bpa1' p a (x : xs, y : ys) = case findInAncestors a x y of
  Nothing         -> Set.empty
  Just (xs', ys') -> Set.union
    (Set.singleton (Set.fromList [(xs, xs'), (ys, ys')]))
    (bpa1' p (Set.delete (x : xs', y : ys') a) (x : xs, y : ys))
bpa1' _ _ _ = Set.empty

findInAncestors :: Ancestors -> Variable -> Variable -> Maybe (Word, Word)
findInAncestors a x y = Set.foldr
  (\p acc -> case acc of
    Just p  -> Just p
    Nothing -> findInPair p x y
  )
  Nothing
  a

findInPair :: (Word, Word) -> Variable -> Variable -> Maybe (Word, Word)
findInPair (x' : xs, y' : ys) x y | x == x' && y == y' = Just (xs, ys)
                                  | otherwise          = Nothing
findInPair _ _ _ = Nothing

bpa2 :: NodeTransformation
bpa2 = applyBpa bpa2'

bpa2' :: Productions -> Ancestors -> (Word, Word) -> Set.Set Node
bpa2' p a (x : xs, y : ys)
  | not (normed p x && normed p y) = Set.empty
  | otherwise = case gammaBPA2 p x y of
    Nothing    -> Set.empty
    Just gamma -> Set.singleton (pairsBPA2 p (x : xs) (y : ys) gamma)
bpa2' _ _ _ = Set.empty

gammaBPA2 :: Productions -> Variable -> Variable -> Maybe Word
gammaBPA2 p x y = throughPath p ls [x1]
  where
    nx = norm p [x]
    ny = norm p [y]
    x0 = if nx <= ny then x else y
    x1 = if nx <= ny then y else x
    ls = pathToSkip p x0

pairsBPA2 :: Productions -> Word -> Word -> Word -> Node
pairsBPA2 p (x : xs) (y : ys) gamma = Set.fromList [p1, p2]
 where
  p1 = if norm p [x] >= norm p [y] then ([x], y : gamma) else (x : gamma, [y])
  p2 = if norm p [x] >= norm p [y] then (gamma ++ xs, ys) else (xs, gamma ++ ys)

-- only applicable to normed variables
pathToSkip :: Productions -> Variable -> [Label]
pathToSkip p x = fst . head $ filter (null . snd) ps
  where ps = pathToSkip' p (Map.assocs $ Map.mapKeys (: []) (transitions x p))

pathToSkip' :: Productions -> [([Label], Word)] -> [([Label], Word)]
pathToSkip' p ps | any (null . snd) ps = ps
                 | otherwise           = pathToSkip' p ps'
 where
  ps' = foldr
    (\(ls, xs) ts ->
      map (\(l, ys) -> (ls ++ [l], ys)) (Map.assocs $ transitions xs p)
        `union` ts
    )
    []
    ps

throughPath :: Productions -> [Label] -> Word -> Maybe Word
throughPath p (l : ls) xs | not (Map.member l ts) = Nothing
                          | otherwise             = throughPath p ls xs'
 where
  ts  = transitions xs p
  xs' = ts Map.! l
throughPath p _ xs = Just xs

-- Pruning nodes (Warning: pruneWord is duplicated from Bisimulation.Norm)

pruneNode :: Productions -> Node -> Node
pruneNode ps = Set.map $ bimap pruneWord pruneWord
  where
    pruneWord :: Word -> Word
    pruneWord = foldr (\x ys -> x : if normed ps x then ys else []) []
