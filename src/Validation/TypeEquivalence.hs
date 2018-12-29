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
, unfold
, subs
) where

import           Control.Monad.State
import           Data.List (isPrefixOf, union)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Queue.Queue as Queue
import           Syntax.Kinds
import           Syntax.Types
import           Validation.Kinding
import           Validation.TypingState (KindEnv)
import           Validation.Grammar
import           Validation.TypeToGrammar
import           Validation.Norm

type Node = Set.Set ([TypeVar], [TypeVar])

type Ancestors = Node

type NodeQueue = Queue.Queue (Node, Ancestors)

type NodeTransformation = Grammar -> Ancestors -> ([TypeVar], [TypeVar]) -> Set.Set Node

expansionTree :: Grammar -> [TypeVar] -> [TypeVar] -> Bool
expansionTree g xs ys = expansionTree' g (Queue.enqueue (Set.singleton (xs, ys), Set.empty) Queue.empty)

expansionTree' :: Grammar -> NodeQueue -> Bool
expansionTree' g q
  | Queue.isEmpty q             = False
  | n == Set.fromList []        = True
  | otherwise                   = case expandNode0 g n of
      Nothing  -> expansionTree' g (Queue.dequeue q)
      Just n'  -> if n' == Set.fromList [([],[])] then True else expansionTree' g (Set.foldr Queue.enqueue (Queue.dequeue q) (simplify g (Set.union a n) n') )
  where (n,a) = Queue.front q

expandNode0 :: Grammar -> Node -> Maybe Node
expandNode0 g n
  | m == Just Set.empty = Nothing
  | otherwise           = m
    where m = expandNode g n

expandNode :: Grammar -> Node -> Maybe Node
expandNode g =
  Set.foldr(\p acc -> case acc of
    Nothing  -> expandPair g p
    Just n'  -> case expandPair g p of
      Nothing  -> Nothing
      Just n'' -> Just (Set.union n' n'')) (Just Set.empty)

expandPair :: Grammar -> ([TypeVar], [TypeVar]) -> Maybe Node
expandPair g (xs, ys)
  | Map.keysSet m1 == Map.keysSet m2 = Just $ match m1 m2
  | otherwise                        = Nothing
  where m1 = transitions g xs
        m2 = transitions g ys

match :: Map.Map Label [TypeVar] -> Map.Map Label [TypeVar] -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Apply the different node transformations

simplify :: Grammar -> Ancestors ->  Node -> Set.Set (Node, Ancestors)
simplify g a n  = Set.union (Set.map (\p -> (p, Set.union a n)) m) (Set.singleton (n'',a))
    where n'  = Set.foldr (\p n -> Set.union (Set.fold Set.union Set.empty (reflex g a p)) n) Set.empty n
          n'' = Set.foldr (\p n -> Set.union (Set.fold Set.union Set.empty (congruence g a p)) n) Set.empty n'
          m   = Set.foldr (\p n -> Set.union (applyBPAs g a (Set.delete p n'') p) n) Set.empty n''
-- Perhaps we need to iterate until reaching a fixed point

-- is applying transf to all elements, should be one at a time
-- should return a list of nodes instead of a node ..?
-- apply :: Grammar -> Ancestors -> NodeTransformation -> Node -> Node
-- apply g a trans = Set.foldr (\p n -> Set.union (trans g a p) n) Set.empty

applyBPAs :: Grammar -> Ancestors -> Node -> ([TypeVar], [TypeVar]) -> Set.Set Node
applyBPAs g a n p = Set.map (\v -> Set.union v n) m
  where m = foldr (\trans l -> Set.union (trans g a p) l ) Set.empty [bpa1,bpa2]

reflex :: NodeTransformation
reflex _ _ (xs, ys)
  | xs == ys  = Set.empty
  | otherwise = Set.singleton (Set.singleton (xs, ys))

congruence :: NodeTransformation
congruence _ a p
  | congruentToAncestors a p = Set.empty
  | otherwise                = Set.singleton (Set.singleton p)

congruentToAncestors :: Ancestors -> ([TypeVar], [TypeVar]) -> Bool
congruentToAncestors a p = or $ Set.map (congruentToPair a p) a

congruentToPair :: Ancestors -> ([TypeVar], [TypeVar]) -> ([TypeVar], [TypeVar]) -> Bool
congruentToPair a (xs, ys) (xs', ys') =
  length xs' > 0 && xs' `isPrefixOf` xs &&
  length ys' > 0 && ys' `isPrefixOf` ys &&
  ( x1 == x2 || congruentToAncestors a (x1, x2) )
  where x1 = drop (length xs') xs
        x2 = drop (length ys') ys

bpa1 :: NodeTransformation
bpa1 g a (x:xs, y:ys) =
  case findInAncestors a x y of
    Nothing         -> Set.empty
    Just (xs', ys') -> Set.union (Set.singleton (Set.fromList [(xs,xs'), (ys,ys')])) (bpa1 g (Set.delete (x:xs', y:ys') a) (x:xs, y:ys))
bpa1 _ _ p = Set.singleton (Set.singleton p)

-- only works for equal norms
bpa2 :: NodeTransformation
bpa2 g a (x:xs, y:ys)
  | m && norm g [x] == norm g [y] = Set.singleton (Set.fromList [([x],[y]), (xs,ys)])
  | otherwise                     = Set.empty
  where m = normed g x && normed g y && (length xs > 0 || length ys > 0)
bpa2 _ _ p = Set.singleton (Set.singleton p)

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
  Map.size m1 == Map.size m2 && Map.foldlWithKey (checkBinding k m2) True m1
--equivalent _ Skip Skip = True
--equivalent _ Skip _ = False
--equivalent _ _ Skip = False
equivalent k t u
  | isSessionType k t && isSessionType k u = expansionTree (normalise g) [x] [y]
  | otherwise = False
  where (x, state)     = convertToGNF initial t
        (y, (g, _, _)) = convertToGNF state u

checkBinding :: KindEnv -> TypeMap -> Bool -> Constructor -> Type -> Bool
checkBinding k m acc l t = acc && l `Map.member` m && equivalent k (m Map.! l) t

-- -- testing

convertTwo :: Type -> Type -> (TypeVar, TypeVar, (Grammar, Visited, Int))
convertTwo t u = (x, y, s)
  where (x, state) = convertToGNF initial t
        (y, s) = convertToGNF state u

-- TODO: move to another folder
-- tests

buildGNF :: Type -> GNF
buildGNF t = evalState (generateGNF t) initial

generateGNF :: Type -> GNFState GNF
generateGNF t = do
  w <- toGNF t
  y <- freshVar
  insertProduction y (MessageLabel In UnitType) w
  p <- getGrammar
  return $ GNF {start = y, productions = p}

s1 = Message Out CharType
t1 = buildGNF s1
s2 = Var "α"
t2 = buildGNF s2
s3 = Semi (Message Out IntType) (Message In BoolType)
t3 = buildGNF s3
s4 = Semi s3 s1
t4 = buildGNF s4
s5 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s1)])
t5 = buildGNF s5
s6 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s3)])
t6 = buildGNF s6
yBind = Bind "y" (Kind {prekind = Session, multiplicity = Lin})
treeSend = Rec yBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
t7 = buildGNF treeSend
t8 = buildGNF $ Semi treeSend (Var "α")
s9 = Rec yBind (Semi s1 (Var "y"))
t9 = buildGNF s9
s10 = Semi s4 (Semi (Semi s3 s1) s4)
t10 = buildGNF s10
s11 = Semi (Rec yBind (Semi treeSend (Var "y"))) treeSend
t11 = buildGNF s11
zBind = Bind "z" (Kind {prekind = Session, multiplicity = Lin})
s12 = Semi (Rec zBind (Semi treeSend (Var "z"))) treeSend
t12 = buildGNF s12
s13 = Semi treeSend Skip
t13 = buildGNF s13
s14 = Semi Skip treeSend
t14 = buildGNF s14
s15 = Semi treeSend treeSend
t15 = buildGNF s15
treeSend1 = Rec zBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "z") (Var "z")))]))
s16 = Semi treeSend treeSend1
t16 = buildGNF s16
s17 = Rec zBind (Semi s1 (Var "z"))
t17 = buildGNF s17
s18 = Rec zBind (Semi s1 (Semi (Var "z") (Var "z")))
t18 = buildGNF s18
s19 = Rec zBind (Semi (Semi s1 (Var "z")) (Var "z"))
t19 = buildGNF s19
s20 = Message In IntType
s21 = Semi s1 (Semi s2 s20)
s22 = Semi (Semi s1 s2) s20
s23 = Semi s1 Skip
s24 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "y")) (Var "z")))
t24 = buildGNF s24
s25 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "z")) (Var "y")))
t25 = buildGNF s25
s26 = Semi (Choice External (Map.fromList [("Leaf", Skip)])) (Var "α")
t26 = buildGNF s26
s27 = Choice External (Map.fromList [("Leaf", (Var "α"))])
t27 = buildGNF s27
s28 = Rec yBind (Choice External (Map.fromList [("Add", Semi (Semi (Var "y") (Var "y")) (Message Out IntType)), ("Const", Skip)]))

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
