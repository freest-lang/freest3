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
) where

import           Control.Monad.State
import           Data.List (isPrefixOf, union, sortBy, reverse, head, break, sortOn)
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
import           Text.Printf

type Node = Set.Set ([TypeVar], [TypeVar])

type Ancestors = Node

type NodeQueue = Queue.Queue (Node, Ancestors)

type NodeTransformation = Productions -> Ancestors -> Node -> Set.Set Node

expansionTree :: Productions -> [TypeVar] -> [TypeVar] -> Bool
expansionTree g xs ys = expansionTree' g (Queue.enqueue (Set.singleton (xs, ys), Set.empty) Queue.empty)

expansionTree' :: Productions -> NodeQueue -> Bool
expansionTree' g q
  | Queue.isEmpty q   = False
  | Set.null n        = True
  | otherwise         = case expandNode g n of
      Nothing  -> expansionTree' g (Queue.dequeue q)
      Just n'  -> if n' == Set.fromList [([],[])] then True else expansionTree' g (simplify g (Set.union a n) n' q)
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

match :: Map.Map Label [TypeVar] -> Map.Map Label [TypeVar] -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Apply the different node transformations

simplify :: Productions -> Ancestors ->  Node -> NodeQueue -> NodeQueue
simplify g a n q = foldr enqueueNode (Queue.dequeue q) s
     where m = findFixedPoint g a (Set.singleton (n,a))
           s  = reverse (sortBy (\(n1,_) (n2,_) -> compare (maximumLength n1) (maximumLength n2)) (Set.toList m))

enqueueNode :: (Node,Ancestors) -> NodeQueue -> NodeQueue
enqueueNode (n,a) q
 | Set.null n           = Queue.priorityEnqueue (n,a) q
 | maximumLength n == 1 = Queue.priorityEnqueue (n,a) q
 | otherwise            = Queue.enqueue (n,a) q

--if we could compare transformations (i.e. t1==t2), we could have a single apply
apply :: Productions -> NodeTransformation -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
apply g trans ns = Set.fold (\(n,a) ns -> Set.union (Set.map (\s -> (s,a)) (trans g a n)) ns) Set.empty ns

findFixedPoint :: Productions -> Ancestors -> Set.Set (Node,Ancestors) -> Set.Set (Node,Ancestors)
findFixedPoint g a nas
  | nas == nas' = nas
  | otherwise = findFixedPoint g a nas'
  where nas' = foldr (apply g) nas [reflex, congruence, bpa1, reflex, congruence, bpa2, reflex, congruence]

-- auxiliar function: returns the maximum length of the pairs in a node
maximumLength :: Node -> Int
maximumLength n = Set.findMax (Set.map (\(a,b) -> max (length a) (length b)) n)

-- node transformations

reflex :: NodeTransformation
reflex _ _ n = Set.singleton (Set.filter (\(xs,ys) -> not ( xs == ys )) n)

congruence :: NodeTransformation
congruence _ a n = Set.singleton (Set.filter (\p -> not (congruentToAncestors a p)) n)

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
bpa1 g a n =
  Set.foldr (\p ps -> Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa1' g a p)) ps) (Set.singleton n) n

bpa1' :: Productions -> Ancestors -> ([TypeVar],[TypeVar]) -> Set.Set Node
bpa1' g a (x:xs,y:ys) =
  case findInAncestors a x y of
    Nothing         -> Set.empty
    Just (xs', ys') -> Set.union (Set.singleton (Set.fromList [(xs,xs'), (ys,ys')])) (bpa1' g (Set.delete (x:xs', y:ys') a) (x:xs, y:ys))
bpa1' _ _ p = Set.empty

-- only works for equal norms
bpa2 :: NodeTransformation
bpa2 g a n =
  Set.foldr (\p ps -> Set.union (Set.map (\v -> Set.union v (Set.delete p n)) (bpa2' g a p)) ps) (Set.singleton n) n

bpa2' :: Productions -> Ancestors -> ([TypeVar],[TypeVar]) -> Set.Set Node
bpa2' g a (x:xs, y:ys)
  | not (normed g x && normed g y) = Set.empty
  | norm g [x] == norm g [y]       = Set.singleton (Set.fromList [([x],[y]), (xs,ys)])
  | m                              = Set.map (pairsBPA2 g (x:xs, y:ys)) gammas
  | otherwise                      = Set.empty
  where m = (not (norm g [x] > norm g [y]) || length ys > 0) && (not (norm g [x] < norm g [y]) || length xs > 0)
        gammas = gammasBPA2 g (x,y)
bpa2' _ _ p = Set.empty

pairsBPA2 :: Productions -> ([TypeVar],[TypeVar]) -> [TypeVar] -> Node
pairsBPA2 g (x:xs, y:ys) gamma = Set.fromList [p1, p2]
  where  p1 = if (norm g [x] >= norm g [y]) then ( [x], [y] ++ gamma ) else ( [x] ++ gamma, [y] )
         p2 = if (norm g [x] >= norm g [y]) then ( gamma ++ xs, ys ) else ( xs, gamma ++ ys )

gammasBPA2 :: Productions -> (TypeVar,TypeVar) -> Set.Set [TypeVar]
gammasBPA2 g (x,y) = gammaSellection g nt diff
  where diff = norm g [x] - norm g [y]
        ks =  Map.keys g
        ks' = sortOn (\x -> index x 0 (length ks + 4)) ks
        nt = if diff >= 0 then snd (splitNonTerminal g ks') else fst (splitNonTerminal g ks')

index :: TypeVar -> Int -> Int -> Int
index x i max
  | i == max     = -1
  | x == ("_x"++show i) = i
  | otherwise    = index x (i+1) max

splitNonTerminal :: Productions -> [TypeVar] -> ([TypeVar],[TypeVar])
splitNonTerminal g ks = break (== (last vs)) ks
  where ts = (map (\k -> transitions g [k]) ks)
        ts' = foldr union [] (map Map.toList ts)
        vs = map (\(l,x) -> head x) (filter (\(a,b) -> show a == "?()") ts')

gammaSellection :: Productions -> [TypeVar] -> Int -> Set.Set [TypeVar]
gammaSellection g xs diff = foldr (\x xs -> Set.union (substringGamma g diff (Set.singleton x)) xs) Set.empty preGammas
  where l = foldr union [] (map (\x -> Map.elems (transitions g [x])) xs)
        l' = filter (all (normed g)) l
        n = filter (\xs -> sum (map (\x -> norm g [x]) xs) >= diff) l'
        preGammas = foldr union [] (map (\l -> map (\i -> drop i l) [0..(length l-1)]) n)

substringGamma :: Productions -> Int -> Set.Set [TypeVar] -> Set.Set [TypeVar]
substringGamma g i ys
  | s == i    = ys
  | s < i     = Set.empty
  | otherwise = substringGamma g i (Set.singleton (take ((length xs) - 1) xs))
  where xs = foldr union [] (Set.toList ys)
        s = sum (map (\x -> norm g [x]) xs)

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
equivalent k t u
  | isSessionType k t && isSessionType k u = expansionTree (normalise g) [x] [y]
  | otherwise = False
  where (g, [x, y]) = convertToGrammar [t, u]

checkBinding :: KindEnv -> TypeMap -> Bool -> Constructor -> Type -> Bool
checkBinding k m acc l t = acc && l `Map.member` m && equivalent k (m Map.! l) t

{-
-- testing
-- TODO: move to another folder

buildGrammar :: Type -> Grammar
buildGrammar t = evalState (generateGrammar t) initial

generateGrammar :: Type -> TransState Grammar
generateGrammar t = do
  w <- toGrammar t
  y <- freshVar
  insertProduction y (MessageLabel In UnitType) w
  p <- getGrammar
  return $ Grammar {start = y, productions = p}

s1 = Message Out CharType
t1 = buildGrammar s1
s2 = Var "α"
t2 = buildGrammar s2
s3 = Semi (Message Out IntType) (Message In BoolType)
t3 = buildGrammar s3
s4 = Semi s3 s1
t4 = buildGrammar s4
s5 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s1)])
t5 = buildGrammar s5
s6 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s3)])
t6 = buildGrammar s6
yBind = Bind "y" (Kind {prekind = Session, multiplicity = Lin})
treeSend = Rec yBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
t7 = buildGrammar treeSend
t8 = buildGrammar $ Semi treeSend (Var "α")
s9 = Rec yBind (Semi s1 (Var "y"))
t9 = buildGrammar s9
s10 = Semi s4 (Semi (Semi s3 s1) s4)
t10 = buildGrammar s10
s11 = Semi (Rec yBind (Semi treeSend (Var "y"))) treeSend
t11 = buildGrammar s11
zBind = Bind "z" (Kind {prekind = Session, multiplicity = Lin})
s12 = Semi (Rec zBind (Semi treeSend (Var "z"))) treeSend
t12 = buildGrammar s12
s13 = Semi treeSend Skip
t13 = buildGrammar s13
s14 = Semi Skip treeSend
t14 = buildGrammar s14
s15 = Semi treeSend treeSend
t15 = buildGrammar s15
treeSend1 = Rec zBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "z") (Var "z")))]))
s16 = Semi treeSend treeSend1
t16 = buildGrammar s16
s17 = Rec zBind (Semi s1 (Var "z"))
t17 = buildGrammar s17
s18 = Rec zBind (Semi s1 (Semi (Var "z") (Var "z")))
t18 = buildGrammar s18
s19 = Rec zBind (Semi (Semi s1 (Var "z")) (Var "z"))
t19 = buildGrammar s19
s20 = Message In IntType
s21 = Semi s1 (Semi s2 s20)
s22 = Semi (Semi s1 s2) s20
s23 = Semi s1 Skip
s24 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "y")) (Var "z")))
t24 = buildGrammar s24
s25 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "z")) (Var "y")))
t25 = buildGrammar s25
s26 = Semi (Choice External (Map.fromList [("Leaf", Skip)])) (Var "α")
t26 = buildGrammar s26
s27 = Choice External (Map.fromList [("Leaf", (Var "α"))])
t27 = buildGrammar s27
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
-}
