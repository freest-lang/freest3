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

module Validation.TypeEquivalence(
  equivalent
) where

import           Control.Monad.State
import           Data.List (isPrefixOf, union)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Queue.Queue as Queue -- Use instead http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html
import           Syntax.Kinds
import           Syntax.Types
import           Validation.Kinding
import           Validation.TypingState (KindEnv)
import           Validation.Grammar
import           Validation.TypeToGrammar
import           Validation.Norm
-- import           Text.Printf

type Node = Set.Set ([TypeVar], [TypeVar])

type Ancestors = Node

type NodeQueue = Queue.Queue (Node, Ancestors)

type NodeTransformation = Productions -> Ancestors -> Node -> Set.Set Node

expansionTree :: Productions -> [TypeVar] -> [TypeVar] -> Bool
expansionTree g xs ys = expansionTree' g (Queue.enqueue (Set.singleton (xs, ys), Set.empty) Queue.empty)

expansionTree' :: Productions -> NodeQueue -> Bool
expansionTree' g q
  | Queue.isEmpty q = False
  | Set.null n      = True
  | otherwise       = case expandNode g n of
      Nothing  -> expansionTree' g (Queue.dequeue q)
      Just n'  -> expansionTree' g (simplify g (Set.union a n) n' q)
  where (n,a) = Queue.front q

expandNode :: Productions -> Node -> Maybe Node
expandNode g =
  Set.foldr(\p acc -> case acc of
    Nothing  -> Nothing
    Just n'  -> case expandPair g p of
      Nothing  -> Nothing
      Just n'' -> Just (Set.union n' n'')) (Just Set.empty)

expandPair :: Productions -> ([TypeVar], [TypeVar]) -> Maybe Node
expandPair g (xs, ys)
  | Map.keysSet m1 == Map.keysSet m2 = Just $ match m1 m2
  | otherwise                        = Nothing
  where m1 = transitions g xs
        m2 = transitions g ys

match :: Transitions -> Transitions -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Prune nodes once expanded
pruneNode :: Productions -> Node ->  Node
pruneNode g = Set.map (\(xs,ys) -> (pruneWord g xs, pruneWord g ys))

-- Apply the different node transformations

simplify :: Productions -> Ancestors ->  Node -> NodeQueue -> NodeQueue
simplify g a n q = foldr enqueueNode (Queue.dequeue q) m'
  where m' = findFixedPoint g (Set.singleton (n,a))

enqueueNode :: (Node,Ancestors) -> NodeQueue -> NodeQueue
enqueueNode (n,a) q
 | Set.null n           = Queue.priorityEnqueue (n,a) q
 | maximumLength n == 1 = Queue.priorityEnqueue (n,a) q
 | otherwise            = Queue.enqueue (n,a) q

apply :: Productions -> NodeTransformation -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
apply g trans ns = Set.fold (\(n,a) ns -> Set.union (Set.map (\s -> (s,a)) (trans g a n)) ns) Set.empty ns

findFixedPoint :: Productions -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
findFixedPoint g nas
  | nas == nas' = nas
  | otherwise = findFixedPoint g nas'
  where nas' = if allNormed g then foldr (apply g) nas [reflex, congruence, bpa2, filtering]
                              else foldr (apply g) nas [reflex, congruence, bpa1, bpa2, filtering]

normsMatch :: Productions -> Node -> Bool
normsMatch g n = and $ Set.map (\(xs,ys) -> sameNorm g xs ys) n

-- The maximum length of the pairs in a node

maximumLength :: Node -> Int
maximumLength n
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

-- TYPE EQUIVALENCE

equivalent :: KindEnv -> Type -> Type -> Bool
equivalent _ (Var x) (Var y) = x == y
equivalent _ (Basic b) (Basic c) = b == c
equivalent k (Fun m t1 t2) (Fun n u1 u2) =
  m == n && equivalent k t1 u1 && equivalent k t2 u2
equivalent k (PairType t1 t2) (PairType u1 u2) =
  equivalent k t1 u1 && equivalent k t2 u2
equivalent k (Datatype m1) (Datatype m2) =
  Map.size m1 == Map.size m2 &&
  Map.foldlWithKey (checkBinding k m2) True m1
equivalent k t u =
  isSessionType k t &&
  isSessionType k u &&
  expansionTree (prune p) [x] [y]
  where Grammar [x, y] p = convertToGrammar [t, u]

checkBinding :: KindEnv -> TypeMap -> Bool -> Constructor -> Type -> Bool
checkBinding k m acc l t = acc && l `Map.member` m && equivalent k (m Map.! l) t

{- testing

alphaKinding = Map.singleton "α" (Kind Session Lin)

e1 = equivalent alphaKinding s1 s1
e2 = equivalent alphaKinding s1 s2 -- False
e3 = equivalent alphaKinding s1 s3 -- False
e4 = equivalent alphaKinding s3 s3
e5 = equivalent alphaKinding s3 s4 -- False
e6 = equivalent alphaKinding s1 s5 -- False
e7 = equivalent alphaKinding s4 s5 -- False
e8 = equivalent alphaKinding s5 s6 -- False
e9 = equivalent alphaKinding s9 s9
e10 = equivalent alphaKinding treeSend treeSend
e11 = equivalent alphaKinding s21 s22
e12 = equivalent alphaKinding s1 s23
e13 = equivalent alphaKinding s24 s24
e14 = equivalent alphaKinding s24 s25
e15 = equivalent alphaKinding s26 s27

-}
