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

type Productions = Map.Map TypeVar (Map.Map Label [TypeVar])

data GNF = GNF {start :: TypeVar, productions :: Productions}

instance Show GNF where
  show g = "start:" ++ start g ++ showProductions (productions g)

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showProds ""

showProds :: TypeVar -> (Map.Map Label [TypeVar]) -> String -> String
showProds x m s = s ++ "\n" ++ Map.foldrWithKey (showProd x) "" m

showProd :: TypeVar -> Label -> [TypeVar] -> String -> String
showProd x l ys s = s ++ "\n" ++ x ++ " ::= " ++ show l ++ " " ++ (if null ys then "ε" else concat ys)

-- The state of the translation to GNF

type Visited = Map.Map Type TypeVar

type GNFState = State (Productions, Visited, Int)

-- State manipulation functions

initial :: (Productions, Visited, Int)
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

getAllProductions :: GNFState Productions
getAllProductions = do
  (p, _, _) <- get
  return p

getProductions :: TypeVar -> GNFState (Map.Map Label [TypeVar])
getProductions x = do
  p <- getAllProductions
  return $ p Map.! x

member :: TypeVar -> GNFState Bool
member x = do
  p <- getAllProductions
  return $ Map.member x p

insertProduction :: TypeVar -> Label -> [TypeVar] -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertProductions :: TypeVar -> (Map.Map Label [TypeVar]) -> GNFState ()
insertProductions x m = insertProductions' x (Map.assocs m)

insertProductions' x [] = return ()
insertProductions' x ((l, w):as) = do
  insertProduction x l w
  insertProductions' x as

replaceInProductions :: [TypeVar] -> TypeVar -> GNFState ()
replaceInProductions w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: Eq a => [a] -> a -> [a] -> [a]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to GNF

-- convertToGNF :: (Productions, Visited, Int) -> Type -> (GNF, (Productions, Visited, Int))
-- -- Assume: t is a session type different from Skip
-- convertToGNF s t = runState (generateGNF t) s
convertToGNF :: (Productions, Visited, Int) -> Type -> (TypeVar, (Productions, Visited, Int))
-- Assume: t is a session type different from Skip
convertToGNF state t = (x, state')
  where ([x], state') = runState (toGNF t) state

generateGNF :: Type -> GNFState GNF
generateGNF t = do
  [y] <- toGNF t
  p <- getAllProductions
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
  (x:xs) <- toGNF t
  ys <- toGNF u
  b <- member x
  if b then do
    m <- getProductions x
    insertProductions x (Map.map (++ xs ++ ys) m)
    return [x]
  else
    return $ (x:xs) ++ ys -- E.g., rec x. !Int;(x;x)
toGNF' (Choice p m) = do
  y <- freshVar
  assocsToGNF y p (Map.assocs m)
  return [y]
toGNF' (Rec b t) = do
  y <- freshVar
  let u = rename (Rec b t) y -- On the fly alpha conversion
  insertVisited u y
  (z:zs) <- toGNF (unfold u)
  replaceInProductions (z:zs) y
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

-- BISIMULATION

type Node = Set.Set ([TypeVar], [TypeVar])

bisim :: [TypeVar] -> [TypeVar] -> Productions -> Bool
bisim xs ys g = check [simplify g [] (Set.singleton (xs, ys))] g

check :: [Node] -> Productions -> Bool
-- Most recent node first
-- Assume: the most recent node is simplified
check (n:ns) g =
  Set.null n ||
  case expandNode n g of
    Nothing -> False
    Just n' -> check ((simplify g ns n'):n:ns) g

expandNode :: Node -> Productions -> Maybe Node
expandNode n g =
  Set.fold(\p acc -> case acc of
    Nothing -> Nothing
    Just s1 -> case expandPair p g of
      Nothing -> Nothing
      Just s2 -> Just (Set.union s1 s2)) (Just Set.empty) n

expandPair :: ([TypeVar], [TypeVar]) -> Productions -> Maybe Node
expandPair (xs, ys) g =
  if Map.keysSet m1 == Map.keysSet m2
  then Just (match m1 m2)
  else Nothing
  where m1 = reduce g xs
        m2 = reduce g ys

reduce :: Productions -> [TypeVar] -> Map.Map Label [TypeVar]
reduce _ []     = Map.empty
reduce g (x:xs) = Map.map (++ xs) (g Map.! x)

match :: Map.Map Label [TypeVar] -> Map.Map Label [TypeVar] -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs node -> Set.insert (xs, m2 Map.! l) node) Set.empty m1

simplify :: Productions -> [Node] -> Node -> Node
simplify p ns = Set.filter (retain p ns)

retain :: Productions -> [Node] -> ([TypeVar], [TypeVar]) -> Bool
retain g ns (xs, ys) = not $
  xs == ys &&
  inCongruenceList ns xs ys &&
  True && -- BPA 1
  True && -- BPA 2
  True    -- Remove weakly normed

inCongruenceList :: [Node] -> [TypeVar] -> [TypeVar] -> Bool
inCongruenceList ns xs ys = or $ map (inCongruenceNode xs ys) ns

inCongruenceNode :: [TypeVar] -> [TypeVar] -> Node -> Bool
inCongruenceNode xs ys n = or $ Set.map (inCongruencePair xs ys) n

inCongruencePair :: [TypeVar] -> [TypeVar] -> ([TypeVar], [TypeVar]) -> Bool
inCongruencePair _ _ ([], _) = False
inCongruencePair _ _ (_, []) = False
inCongruencePair xs ys (xs', ys')
  | xs == xs' && ys == ys' = True
  | otherwise = head xs' == head ys' && inCongruencePair xs ys (tail xs', tail ys')

-- TYPE EQUIVALENCE

equivalent :: KindEnv -> Type -> Type -> Bool
equivalent _ Skip Skip = True
equivalent _ Skip _    = False
equivalent _ _    Skip = False
equivalent _ t u = bisim [x] [y] p
  where (x, state)     = convertToGNF initial t
        (y, (p, _, _)) = convertToGNF state u

-- testing

-- convertTwo :: Type -> Type -> (TypeVar, TypeVar, Productions)
-- convertTwo t u = (start g, start h, productions h)
--   where (g, state) = convertToGNF initial t
--         (h, _    ) = convertToGNF state u

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
