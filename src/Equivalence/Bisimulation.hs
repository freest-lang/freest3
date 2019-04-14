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

module Equivalence.Bisimulation
( equivalent
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Equivalence.Grammar
import           Equivalence.TypeToGrammar
import           Equivalence.Norm
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import           Data.List (isPrefixOf)

equivalent :: Type -> Type -> Bool
equivalent t u = expand (prune p) [x] [y]
  where Grammar [x, y] p = convertToGrammar [t, u]

type Node = Set.Set ([TypeVar], [TypeVar])

type Ancestors = Node

type NodeQueue = Sequence.Seq (Node, Ancestors)

type NodeTransformation = Productions -> Ancestors -> Node -> Set.Set Node

expand :: Productions -> [TypeVar] -> [TypeVar] -> Bool
expand ps xs ys = expand' ps (Sequence.singleton (Set.singleton (xs, ys), Set.empty))

expand' :: Productions -> NodeQueue -> Bool
expand' ps ((n, a) Sequence.:<| q')
  | Set.null n      = True
  | otherwise       = case expandNode ps n of
      Nothing -> expand' ps q'
      Just n' -> expand' ps (simplify ps n' (Set.union a n) q')
expand' ps Sequence.Empty = False

expandNode :: Productions -> Node -> Maybe Node
expandNode ps =
  Set.foldr(\p acc -> case acc of
    Nothing  -> Nothing
    Just n'  -> case expandPair ps p of
      Nothing  -> Nothing
      Just n'' -> Just (Set.union n' n'')) (Just Set.empty)

expandPair :: Productions -> ([TypeVar], [TypeVar]) -> Maybe Node
expandPair ps (xs, ys)
  | Map.keysSet m1 == Map.keysSet m2 = Just $ match m1 m2
  | otherwise                        = Nothing
  where m1 = transitions ps xs
        m2 = transitions ps ys

match :: Transitions -> Transitions -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Prune nodes once expanded
pruneNode :: Productions -> Node ->  Node
pruneNode ps = Set.map (\(xs,ys) -> (pruneWord ps xs, pruneWord ps ys))

-- Apply the different node transformations

simplify :: Productions -> Node -> Ancestors -> NodeQueue -> NodeQueue
simplify ps n a q =
  foldr enqueueNode q (findFixedPoint ps (Set.singleton (n, a)))

enqueueNode :: (Node,Ancestors) -> NodeQueue -> NodeQueue
enqueueNode (n,a) q
 | maxLength n <= 1 = (n,a) Sequence.<| q
 | otherwise        = q Sequence.|> (n,a)

apply :: Productions -> NodeTransformation -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
apply ps trans ns = Set.fold (\(n,a) ns -> Set.union (Set.map (\s -> (s,a)) (trans ps a n)) ns) Set.empty ns

findFixedPoint :: Productions -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
findFixedPoint ps nas
  | nas == nas' = nas
  | otherwise = findFixedPoint ps nas'
  where nas' = if allNormed ps
                 then foldr (apply ps) nas [reflex, congruence, bpa2, filtering]
                 else foldr (apply ps) nas [reflex, congruence, bpa1, bpa2, filtering]

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

filtering :: NodeTransformation
filtering p _ n
  | normsMatch p n = Set.singleton(n)
  | otherwise      = Set.empty

congruentToAncestors :: Ancestors -> ([TypeVar], [TypeVar]) -> Bool
congruentToAncestors a p = or $ Set.map (congruentToPair a p) a

congruentToPair :: Ancestors -> ([TypeVar], [TypeVar]) -> ([TypeVar], [TypeVar]) -> Bool
congruentToPair a (xs, ys) (xs', ys') =
  not (null xs') && xs' `isPrefixOf` xs &&
  not (null ys') && ys' `isPrefixOf` ys &&
  (xs'' == ys'' || congruentToAncestors a (xs'', ys''))
  where xs'' = drop (length xs') xs
        ys'' = drop (length ys') ys

bpa1 :: NodeTransformation
bpa1 g a n =
  Set.foldr (\p ps -> Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa1' g a p)) ps) (Set.singleton n) n

bpa1' :: Productions -> Ancestors -> ([TypeVar],[TypeVar]) -> Set.Set Node
bpa1' p a (x:xs,y:ys) = case findInAncestors a x y of
      Nothing         -> Set.empty
      Just (xs', ys') -> Set.union
                            (Set.singleton (Set.fromList [(xs,xs'), (ys,ys')]))
                            (bpa1' p (Set.delete (x:xs', y:ys') a) (x:xs, y:ys))
bpa1' _ _ p = Set.empty

bpa2 :: NodeTransformation
bpa2 g a n =
  Set.foldr (\p ps ->
                Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa2' g a p))
                          ps) (Set.singleton n) n

bpa2' :: Productions -> Ancestors -> ([TypeVar],[TypeVar]) -> Set.Set Node
bpa2' p a (x:xs, y:ys)
  | not (normed p x && normed p y) = Set.empty
  | otherwise  = case gammaBPA2 p (x,y) of
      Nothing    -> Set.empty
      Just gamma -> Set.singleton (pairsBPA2 p (x:xs, y:ys) gamma)
bpa2' _ _ _ = Set.empty

pairsBPA2 :: Productions -> ([TypeVar],[TypeVar]) -> [TypeVar] -> Node
pairsBPA2 p (x:xs, y:ys) gamma = Set.fromList [p1, p2]
  where  p1 = if (norm p [x] >= norm p [y]) then ( [x], [y] ++ gamma ) else ( [x] ++ gamma, [y] )
         p2 = if (norm p [x] >= norm p [y]) then ( gamma ++ xs, ys ) else ( xs, gamma ++ ys )

gammaBPA2 :: Productions -> (TypeVar,TypeVar) -> Maybe [TypeVar]
gammaBPA2 p (x,y) = throughPath p ls [x1]
 where x0 = if norm p [x] <= norm p [y] then x else y
       x1 = if norm p [x] <= norm p [y] then y else x
       ls = pathToSkip p x0

findInAncestors :: Ancestors -> TypeVar -> TypeVar -> Maybe ([TypeVar], [TypeVar])
findInAncestors a x y =
 Set.foldr (\p acc -> case acc of
   Just p  -> Just p
   Nothing -> findInPair p x y) Nothing a

findInPair :: ([TypeVar], [TypeVar]) -> TypeVar -> TypeVar -> Maybe ([TypeVar], [TypeVar])
findInPair ((x':xs), (y':ys)) x y
  | x == x' && y == y' = Just (xs, ys)
  | otherwise          = Nothing
findInPair _ _ _       = Nothing
