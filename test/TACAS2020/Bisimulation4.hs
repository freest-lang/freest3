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

module TACAS2020.Bisimulation4
( bisimilar,
  bisimilarGrammar -- Test only
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.TypeVariables
import           Equivalence.Grammar
import           TACAS2020.TypeToGrammar
import           Equivalence.Norm
import           Equivalence.Normalisation
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Queue
import           Data.List (isPrefixOf)
import           Prelude hiding (Word) -- Word is (re)defined in module Equivalence.Grammar
import           Debug.Trace

bisimilarGrammar :: Grammar -> Bool
bisimilarGrammar (Grammar [xs, ys] p) = expand (xs, ys) (prune p)

bisimilar :: TypeEnv -> Type -> Type -> Bool
bisimilar tEnv t u = expand (xs, ys) (prune p)
  where Grammar [xs, ys] p = trace (show t ++ "\nbisim\n" ++ show u) $ convertToGrammar tEnv [t, u]

type Node = Set.Set (Word, Word)

type Ancestors = Node

type NodeQueue = Queue.Seq (Node, Ancestors)

expand :: (Word, Word) -> Productions -> Bool
expand p = expand' (Queue.singleton (Set.singleton p, Set.empty))
  where
  expand' :: NodeQueue -> Productions -> Bool
  expand' ((n, a) Queue.:<| q) ps
    | Set.null n      = True
    | otherwise       = case expandNode ps n of
        Nothing -> expand' q ps
        Just n' -> expand' (simplify ps ls n' (Set.union a n) q) ps
    where ls = if allNormed ps
                 then [reflex, congruence, bpa2{--, filtering--}]
                 else [reflex, congruence, bpa1, bpa2{--, filtering--}]

  expand' Queue.Empty _ = False

expandNode :: Productions -> Node -> Maybe Node
expandNode ps =
  Set.foldr(\p acc -> case acc of
    Nothing  -> Nothing
    Just n'  -> case expandPair ps p of
      Nothing  -> Nothing
      Just n'' -> Just (Set.union n' n'')) (Just Set.empty)

expandPair :: Productions -> (Word, Word) -> Maybe Node
expandPair ps (xs, ys)
  | Map.keysSet m1 == Map.keysSet m2 = Just $ match m1 m2
  | otherwise                        = Nothing
  where m1 = transitions xs ps
        m2 = transitions ys ps

match :: Transitions -> Transitions -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Prune nodes once expanded
pruneNode :: Productions -> Node ->  Node
pruneNode ps = Set.map (\(xs,ys) -> (pruneWord ps xs, pruneWord ps ys))

prune :: Productions -> Productions
prune p = Map.map (Map.map (pruneWord p)) p

pruneWord :: Productions -> Word -> Word
pruneWord p = foldr (\x ys -> if normed p x then x:ys else [x]) []

-- Apply the different node transformations

simplify :: Productions -> [NodeTransformation] -> Node -> Ancestors -> NodeQueue -> NodeQueue
simplify ps ls n a q =
  foldr enqueueNode q (findFixedPoint ps ls (Set.singleton (n, a)))

enqueueNode :: (Node,Ancestors) -> NodeQueue -> NodeQueue
enqueueNode (n,a) q
 | maxLength n <= 1 = (n,a) Queue.<| q
 | otherwise        = q Queue.|> (n,a)

type NodeTransformation = Productions -> Ancestors -> Node -> Set.Set Node

apply :: Productions -> NodeTransformation -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
apply ps trans ns =
  Set.fold (\(n,a) ns -> Set.union (Set.map (\s -> (s,a)) (trans ps a n)) ns) Set.empty ns

findFixedPoint :: Productions -> [NodeTransformation] -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
findFixedPoint ps ls nas = nas'
  -- | nas == nas' = nas
  -- | otherwise   = findFixedPoint ps ls nas'
  where nas' = foldr (apply ps) nas ls

normsMatch :: Productions -> Node -> Bool
normsMatch ps n = and $ Set.map (\(xs,ys) -> sameNorm ps xs ys) n

-- The maximum length of the pairs in a node

maxLength :: Node -> Int
maxLength n
  | Set.null n = 0
  | otherwise  = Set.findMax (Set.map (\(a,b) -> max (length a) (length b)) n)

-- The various node transformations

reflex :: NodeTransformation
reflex _ _ = Set.singleton . Set.filter (uncurry (/=))

congruence :: NodeTransformation
congruence _ a = Set.singleton . Set.filter (not . congruentToAncestors a)

congruentToAncestors :: Ancestors -> (Word, Word) -> Bool
congruentToAncestors a p = or $ Set.map (congruentToPair a p) a

congruentToPair :: Ancestors -> (Word, Word) -> (Word, Word) -> Bool
congruentToPair a (xs, ys) (xs', ys') =
  not (null xs') && xs' `isPrefixOf` xs &&
  not (null ys') && ys' `isPrefixOf` ys &&
  (xs'' == ys'' || congruentToAncestors a (xs'', ys''))
  where xs'' = drop (length xs') xs
        ys'' = drop (length ys') ys

filtering :: NodeTransformation
filtering p _ n
  | normsMatch p n = Set.singleton n
  | otherwise      = Set.empty

bpa1 :: NodeTransformation
bpa1 g a n =
  Set.foldr (\p ps -> Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa1' g a p)) ps) (Set.singleton n) n

bpa1' :: Productions -> Ancestors -> (Word,Word) -> Set.Set Node
bpa1' p a (x:xs,y:ys) = case findInAncestors a x y of
      Nothing         -> Set.empty
      Just (xs', ys') -> Set.union
                            (Set.singleton (Set.fromList [(xs,xs'), (ys,ys')]))
                            (bpa1' p (Set.delete (x:xs', y:ys') a) (x:xs, y:ys))
bpa1' _ _ _ = Set.empty

findInAncestors :: Ancestors -> TypeVar -> TypeVar -> Maybe (Word, Word)
findInAncestors a x y =
 Set.foldr (\p acc -> case acc of
   Just p  -> Just p
   Nothing -> findInPair p x y) Nothing a

findInPair :: (Word, Word) -> TypeVar -> TypeVar -> Maybe (Word, Word)
findInPair ((x':xs), (y':ys)) x y
  | x == x' && y == y' = Just (xs, ys)
  | otherwise          = Nothing
findInPair _ _ _       = Nothing

bpa2 :: NodeTransformation
bpa2 g a n =
  Set.foldr (\p ps ->
                Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa2' g a p))
                          ps) (Set.singleton n) n

bpa2' :: Productions -> Ancestors -> (Word,Word) -> Set.Set Node
bpa2' p a (x:xs, y:ys)
  | not (normed p x && normed p y) = Set.empty
  | otherwise  = case gammaBPA2 p x y  of
      Nothing    -> Set.empty
      Just gamma -> Set.singleton (pairsBPA2 p (x:xs) (y:ys) gamma)
bpa2' _ _ _ = Set.empty

gammaBPA2 :: Productions -> TypeVar -> TypeVar -> Maybe Word
gammaBPA2 p x y = throughPath p ls [x1]
 where x0 = if norm p [x] <= norm p [y] then x else y
       x1 = if norm p [x] <= norm p [y] then y else x
       ls = pathToSkip p x0

pairsBPA2 :: Productions -> Word -> Word -> Word -> Node
pairsBPA2 p (x:xs) (y:ys) gamma = Set.fromList [p1, p2]
  where  p1 = if norm p [x] >= norm p [y] then ([x], y : gamma) else (x : gamma, [y])
         p2 = if norm p [x] >= norm p [y] then (gamma ++ xs, ys) else (xs, gamma ++ ys)
