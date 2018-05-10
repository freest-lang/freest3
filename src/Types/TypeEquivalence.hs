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

module Types.TypeEquivalence(
  equivalent
, unfold
, subs  
) where

import Types.Types
import Types.Kinds
import Types.Kinding
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.List(isPrefixOf)

-- GREIBACH NORMAL FORMS

-- type Vars = [TypeVar]

data Label =
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

type Grammar = Map.Map TypeVar (Map.Map Label [TypeVar])

data GNF = GNF {start :: TypeVar, productions :: Grammar}

instance Show GNF where
  show g = "start:" ++ start g ++ showGrammar (productions g)

showGrammar :: Grammar -> String
showGrammar = Map.foldrWithKey showProds ""

showProds :: TypeVar -> (Map.Map Label [TypeVar]) -> String -> String
showProds x m s = s ++ "\n" ++ Map.foldrWithKey (showProd x) "" m

showProd :: TypeVar -> Label -> [TypeVar] -> String -> String
showProd x l ys s = s ++ "\n" ++ x ++ " ::= " ++ show l ++ " " ++ (if null ys then "ε" else concat ys)

-- The state of the translation to GNF

type Visited = Map.Map Type TypeVar

type GNFState = State (Grammar, Visited, Int)

-- State manipulation functions

initial :: (Grammar, Visited, Int)
initial = (Map.empty, Map.empty, 0)

freshVar :: GNFState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n+1))
  return $ "_x" ++ show n

lookupVisited :: Type -> GNFState (Maybe TypeVar)
lookupVisited t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> GNFState ()
insertVisited t x =
  modify (\(p, v, n) -> (p, Map.insert t x v, n))

getAllGrammar :: GNFState Grammar
getAllGrammar = do
  (p, _, _) <- get
  return p

getGrammar :: TypeVar -> GNFState (Map.Map Label [TypeVar])
getGrammar x = do
  p <- getAllGrammar
  return $ p Map.! x

member :: TypeVar -> GNFState Bool
member x = do
  p <- getAllGrammar
  return $ Map.member x p

insertProduction :: TypeVar -> Label -> [TypeVar] -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertGrammar :: TypeVar -> (Map.Map Label [TypeVar]) -> GNFState ()
insertGrammar x m = insertGrammar' x (Map.assocs m)

insertGrammar' x [] = return ()
insertGrammar' x ((l, w):as) = do
  insertProduction x l w
  insertGrammar' x as

replaceInGrammar :: [TypeVar] -> TypeVar -> GNFState ()
replaceInGrammar w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: Eq a => [a] -> a -> [a] -> [a]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to GNF

-- convertToGNF :: (Grammar, Visited, Int) -> Type -> (GNF, (Grammar, Visited, Int))
-- -- Assume: t is a session type different from Skip
-- convertToGNF s t = runState (generateGNF t) s
convertToGNF :: (Grammar, Visited, Int) -> Type -> (TypeVar, (Grammar, Visited, Int))
-- Assume: t is a session type different from Skip
convertToGNF state t = (x, state')
  where ([x], state') = runState (toGNF t) state

generateGNF :: Type -> GNFState GNF
generateGNF t = do
  [y] <- toGNF t
  p <- getAllGrammar
  return $ GNF {start = y, productions = p}

toGNF :: Type -> GNFState [TypeVar]
toGNF t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGNF' t
    Just x ->  return [x]

toGNF' :: Type -> GNFState [TypeVar]
toGNF' Skip =
  return []
toGNF' (Message p b) = do
  y <- freshVar
  insertProduction y (MessageLabel p b) []
  return [y]
toGNF' (Var a) = do -- This is a free variable
  y <- freshVar
  insertProduction y (VarLabel a) []
  return [y]
toGNF' (Semi t u) = do
  xs <- toGNF t
  ys <- toGNF u
  if null xs
  then return ys
  else do
    let (x:xs') = xs
    b <- member x
    if b then do
      m <- getGrammar x
      insertGrammar x (Map.map (++ xs' ++ ys) m)
      return [x]
    else
      return $ xs ++ ys -- E.g., rec x. !Int;(x;x)
toGNF' (Choice p m) = do
  y <- freshVar
  assocsToGNF y p (Map.assocs m)
  return [y]
toGNF' (Rec b t) = do
  y <- freshVar
  let u = rename (Rec b t) y -- On the fly alpha conversion
  insertVisited u y
  (z:zs) <- toGNF (unfold u)
  replaceInGrammar (z:zs) y
  return [z]

assocsToGNF :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> GNFState ()
assocsToGNF _ _ [] = return ()
assocsToGNF y p ((l, t):as) = do
  w <- toGNF t
  insertProduction y (ChoiceLabel p l) w  
  assocsToGNF y p as

-- tests

buildGNF :: Type -> GNF
buildGNF t = evalState (generateGNF t) initial

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
s23 = Semi Skip s1

-- BISIMULATION

type Node = Set.Set ([TypeVar], [TypeVar])

type History = [Node]

bisim :: [TypeVar] -> [TypeVar] -> Grammar -> Bool
bisim xs ys g = check [simplify g [] (Set.singleton (xs, ys))] g

check :: History -> Grammar -> Bool
-- Most recent node first
-- Assume: the most recent node is simplified
check (n:ns) g
  | Set.null n = True
  | otherwise  = case expandNode n g of
      Nothing -> False
      Just n' -> check ((simplify g (n:ns) n'):n:ns) g

expandNode :: Node -> Grammar -> Maybe Node
expandNode n g =
  Set.foldr(\p acc -> case acc of
    Nothing  -> Nothing
    Just ns1 -> case expandPair p g of
      Nothing  -> Nothing
      Just ns2 -> Just (Set.union ns1 ns2)) (Just Set.empty) n

expandPair :: ([TypeVar], [TypeVar]) -> Grammar -> Maybe Node
expandPair (xs, ys) g =
  if Map.keysSet m1 == Map.keysSet m2
  then Just (match m1 m2)
  else Nothing
  where m1 = reduce g xs
        m2 = reduce g ys

reduce :: Grammar -> [TypeVar] -> Map.Map Label [TypeVar]
reduce _ []     = Map.empty    -- This should not happen
reduce g (x:xs) = Map.map (++ xs) (g Map.! x)

match :: Map.Map Label [TypeVar] -> Map.Map Label [TypeVar] -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

simplify :: Grammar -> History -> Node -> Node
simplify g h n = foldr (apply g h) n [bpa1, reflex, congruence]
-- Perhaps we need to iterate until reaching a fixed point

-- The different node transformations

type NodeTransformation = Grammar -> History -> ([TypeVar], [TypeVar]) -> Node

apply :: Grammar -> History -> NodeTransformation -> Node -> Node
apply g h trans = Set.foldr (\p n -> Set.union (trans g h p) n) Set.empty

reflex :: NodeTransformation
reflex _ _ (xs, ys)
  | xs == ys  = Set.empty
  | otherwise = Set.singleton (xs, ys)

congruence :: NodeTransformation
congruence _ h p
  | inCongruenceList h p = Set.empty
  | otherwise            = Set.singleton p

inCongruenceList :: History -> ([TypeVar], [TypeVar]) -> Bool
inCongruenceList h p = or $ map (inCongruenceNode p) h

inCongruenceNode :: ([TypeVar], [TypeVar]) -> Node -> Bool
inCongruenceNode p n = or $ Set.map (inCongruencePair p) n

inCongruencePair :: ([TypeVar], [TypeVar]) -> ([TypeVar], [TypeVar]) -> Bool
inCongruencePair (xs, ys) (xs', ys') =
  xs `isPrefixOf` xs' &&
  ys `isPrefixOf` ys' &&
  (drop (length xs) xs') == (drop (length ys) ys')
-- inCongruencePair _ ([], _) = False
-- inCongruencePair _ (_, []) = False
-- inCongruencePair (xs, ys) (xs', ys')
--   | xs == xs' && ys == ys' = True
--   | otherwise = head xs' == head ys' && inCongruencePair (xs, ys) (tail xs', tail ys')

bpa1 :: NodeTransformation
bpa1 g h (x:xs, y:ys) =
  case findInHistory h x y of
    Nothing         -> Set.singleton (x:xs, y:ys)
    Just (xs', ys') -> Set.fromList [(xs,xs'), (ys,ys')]
bpa1 _ _ p = Set.singleton p

findInHistory :: History -> TypeVar -> TypeVar -> Maybe ([TypeVar], [TypeVar])
findInHistory h x y =
 foldr (\n acc -> case acc of
   Just p  -> Just p
   Nothing -> findInNode n x y) Nothing h

findInNode :: Node -> TypeVar -> TypeVar -> Maybe ([TypeVar], [TypeVar])
findInNode n x y =
  Set.foldr (\((x':xs), (y':ys)) acc -> case acc of
    Just p  -> Just p
    Nothing -> if x == x' && y == y' then Just (xs, ys) else Nothing) Nothing n

-- TYPE EQUIVALENCE

equivalent :: KindEnv -> Type -> Type -> Bool
equivalent k t u
  | isSessionType k t && isSessionType k u = equivalent' t u
  | otherwise = True
  
equivalent' Skip Skip = True
equivalent' Skip _    = False
equivalent' _    Skip = False
equivalent' t u = bisim [x] [y] p
  where (x, state)     = convertToGNF initial t
        (y, (p, _, _)) = convertToGNF state u

-- testing

convertTwo :: Type -> Type -> (TypeVar, TypeVar, Grammar)
convertTwo t u = (x, y, g)
  where (x, state)   = convertToGNF initial t
        (y, (g,_,_)) = convertToGNF state u

e1 = equivalent Map.empty s1 s1
e2 = equivalent Map.empty s1 s2 -- False
e3 = equivalent Map.empty s1 s3 -- False
e4 = equivalent Map.empty s3 s3
e5 = equivalent Map.empty s3 s4 -- False
e6 = equivalent Map.empty s1 s5 -- False
e7 = equivalent Map.empty s4 s5 -- False
e8 = equivalent Map.empty s5 s6 -- False
e9 = equivalent Map.empty s9 s9
e10 = equivalent Map.empty treeSend treeSend
e11 = equivalent Map.empty s21 s22
e12 = equivalent Map.empty s1 s23


-- UNFOLDING, RENAMING, SUBSTITUTING

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec b t) = subs (Rec b t) (var b) t

rename :: Type -> TypeVar -> Type
-- Assumes parameter is a Rec type
rename (Rec (Bind x k) t) y = Rec (Bind y k) (subs (Var y) x t)

subs :: Type -> TypeVar -> Type -> Type -- t[x/u]
subs t y (Var x)
    | x == y              = t
    | otherwise           = Var x
subs t y (Semi t1 t2)     = Semi (subs t y t1) (subs t y t2)
subs t y (PairType t1 t2) = PairType (subs t y t1) (subs t y t2)
-- Assume y /= x 
subs t2 y (Rec b t1)
    | var b == y          = Rec b t1
    | otherwise           = Rec b (subs t2 y t1)
subs t y (Choice v m)     = Choice v (Map.map(subs t y) m)
subs t y (Fun m t1 t2)    = Fun m (subs t y t1) (subs t y t2)
subs _ _ t                = t
