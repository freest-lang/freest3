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
import           Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Syntax.Kinds
import           Syntax.Types
import           Validation.Kinding
import           Validation.TypingState (KindEnv)

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

-- State manipulating functions

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

getGrammar :: GNFState Grammar
getGrammar = do
  (p, _, _) <- get
  return p

getProductions :: TypeVar -> GNFState (Map.Map Label [TypeVar])
getProductions x = do
  p <- getGrammar
  return $ p Map.! x

member :: TypeVar -> GNFState Bool
member x = do
  p <- getGrammar
  return $ Map.member x p

insertProduction :: TypeVar -> Label -> [TypeVar] -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertGrammar :: TypeVar -> (Map.Map Label [TypeVar]) -> GNFState ()
insertGrammar x m = insertGrammar' x (Map.assocs m)

insertGrammar' :: TypeVar -> [(Label, [TypeVar])] -> GNFState ()
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

-- Normalisation

normalise :: Grammar -> Grammar
normalise g = Map.map (Map.map (normaliseWord g)) g

normaliseWord :: Grammar -> [TypeVar] -> [TypeVar]
normaliseWord _ []     = []
normaliseWord g (x:xs)
  | normed g x = x : normaliseWord g xs
  | otherwise  = [x]

normed :: Grammar -> TypeVar -> Bool
normed g x = normedWord g Set.empty [x]

normedWord :: Grammar -> Set.Set TypeVar -> [TypeVar] -> Bool
normedWord _ _ []     = True
normedWord g v (x:xs) =
  not (x `Set.member` v) &&
  any id (map (normedWord g (Set.insert x v)) (Map.elems (transitions g (x:xs))))

-- Conversion to GNF

convertToGNF :: (Grammar, Visited, Int) -> Type -> (TypeVar, (Grammar, Visited, Int))
-- Assume: t is a session type different from Skip
convertToGNF state t = (x, state')
  where ([x], state') = runState (toGNF t) state

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
--  else if null ys
--  then return xs
  else do
    let (x:xs') = xs
    b <- member x
    if b then do
      m <- getProductions x
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

-- buildGNF :: Type -> GNF
-- buildGNF t = evalState (generateGNF t) initial

-- generateGNF :: Type -> GNFState GNF
-- generateGNF t = do
--   [y] <- toGNF t
--   p <- getGrammar
--   return $ GNF {start = y, productions = p}

-- s1 = Message Out CharType
-- t1 = buildGNF s1
-- s2 = Var "α"
-- t2 = buildGNF s2
-- s3 = Semi (Message Out IntType) (Message In BoolType)
-- t3 = buildGNF s3
-- s4 = Semi s3 s1
-- t4 = buildGNF s4
-- s5 = Choice External (Map.fromList
--   [("Leaf", Skip),
--    ("Node", s1)])
-- t5 = buildGNF s5
-- s6 = Choice External (Map.fromList
--   [("Leaf", Skip),
--    ("Node", s3)])
-- t6 = buildGNF s6
-- yBind = Bind "y" (Kind {prekind = Session, multiplicity = Lin})
-- treeSend = Rec yBind (Choice External (Map.fromList
--   [("Leaf",Skip),
--    ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
-- t7 = buildGNF treeSend
-- t8 = buildGNF $ Semi treeSend (Var "α")
-- s9 = Rec yBind (Semi s1 (Var "y"))
-- t9 = buildGNF s9
-- s10 = Semi s4 (Semi (Semi s3 s1) s4)
-- t10 = buildGNF s10
-- s11 = Semi (Rec yBind (Semi treeSend (Var "y"))) treeSend
-- t11 = buildGNF s11
-- zBind = Bind "z" (Kind {prekind = Session, multiplicity = Lin})
-- s12 = Semi (Rec zBind (Semi treeSend (Var "z"))) treeSend
-- t12 = buildGNF s12
-- s13 = Semi treeSend Skip
-- t13 = buildGNF s13
-- s14 = Semi Skip treeSend
-- t14 = buildGNF s14
-- s15 = Semi treeSend treeSend
-- t15 = buildGNF s15
-- treeSend1 = Rec zBind (Choice External (Map.fromList
--   [("Leaf",Skip),
--    ("Node", Semi (Message Out IntType) (Semi (Var "z") (Var "z")))]))
-- s16 = Semi treeSend treeSend1
-- t16 = buildGNF s16
-- s17 = Rec zBind (Semi s1 (Var "z"))
-- t17 = buildGNF s17
-- s18 = Rec zBind (Semi s1 (Semi (Var "z") (Var "z")))
-- t18 = buildGNF s18
-- s19 = Rec zBind (Semi (Semi s1 (Var "z")) (Var "z"))
-- t19 = buildGNF s19
-- s20 = Message In IntType
-- s21 = Semi s1 (Semi s2 s20)
-- s22 = Semi (Semi s1 s2) s20
-- s23 = Semi s1 Skip
-- s24 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "y")) (Var "z")))
-- t24 = buildGNF s24
-- s25 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "z")) (Var "y")))
-- t25 = buildGNF s25
-- s26 = Semi (Choice External (Map.fromList [("Leaf", Skip)])) (Var "α")
-- t26 = buildGNF s26
-- s27 = Choice External (Map.fromList [("Leaf", (Var "α"))])
-- t27 = buildGNF s27
-- s28 = Rec yBind (Choice External (Map.fromList [("Add", Semi (Semi (Var "y") (Var "y")) (Message Out IntType)), ("Const", Skip)]))

-- BISIMULATION

type Node = Set.Set ([TypeVar], [TypeVar])

type Ancestors = Node

bisim :: Grammar -> [TypeVar] -> [TypeVar] -> Bool
bisim g xs ys = bisim' g Set.empty  (Set.singleton (xs, ys))

bisim' :: Grammar -> Ancestors -> Node -> Bool
bisim' g a n
  | Set.null n' = True
  | otherwise  = case expandNode g n' of
      Nothing  -> False
      Just n'' -> bisim' g (Set.union n' a) n''
  where n' = simplify g a n

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

-- this is an op on grammars
transitions :: Grammar -> [TypeVar] -> Map.Map Label [TypeVar]
transitions _ []     = Map.empty
transitions g (x:xs) = Map.map (++ xs) (g Map.! x)

match :: Map.Map Label [TypeVar] -> Map.Map Label [TypeVar] -> Node
match m1 m2 =
  Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

-- Apply the different node transformations

simplify :: Grammar -> Ancestors -> Node -> Node
simplify g a n = foldr (apply g a) n [reflex, congruence, bpa1, bpa3, bpa3]
-- Perhaps we need to iterate until reaching a fixed point

type NodeTransformation = Grammar -> Ancestors -> ([TypeVar], [TypeVar]) -> Node

apply :: Grammar -> Ancestors -> NodeTransformation -> Node -> Node
apply g a trans = Set.foldr (\p n -> Set.union (trans g a p) n) Set.empty

reflex :: NodeTransformation
reflex _ _ (xs, ys)
  | xs == ys  = Set.empty
  | otherwise = Set.singleton (xs, ys)

congruence :: NodeTransformation
congruence _ a p
  | congruentToAncestors a p = Set.empty
  | otherwise                = Set.singleton p

congruentToAncestors :: Ancestors -> ([TypeVar], [TypeVar]) -> Bool
congruentToAncestors a p = or $ Set.map (congruentToPair p) a

congruentToPair :: ([TypeVar], [TypeVar]) -> ([TypeVar], [TypeVar]) -> Bool
congruentToPair (xs, ys) (xs', ys') =
  xs `isPrefixOf` xs' &&
  ys `isPrefixOf` ys' &&
  drop (length xs) xs' == drop (length ys) ys'

-- Needed in this case:
--   [("α", SL)]  |-  rec x . +{A: α, B: x; α} ~ rec y . +{A: Skip, B: y}; α
bpa1 :: NodeTransformation
bpa1 _ a (x:xs, y:ys) =
  case findInAncestors a x y of
    Nothing         -> Set.singleton (x:xs, y:ys)
    Just (xs', ys') -> Set.fromList [(xs,xs'), (ys,ys')]
bpa1 _ _ p = Set.singleton p

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

-- vv made this rule. Sound?
bpa3 :: NodeTransformation
bpa3 _ a (x:xs, y:ys) =
  case findInAncestors a x y of
    Nothing         -> Set.singleton (x:xs, y:ys)
    Just (xs', ys') -> Set.fromList [(xs'++xs, ys'++ys)]
    -- Just ([], [])   -> Set.fromList [(xs, ys)]
    -- Just (xs', ys') -> Set.fromList [(x:xs, y:ys)]
bpa3 _ _ p = Set.singleton p

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
equivalent _ Skip Skip = True
equivalent _ Skip _ = False
equivalent _ _ Skip = False
equivalent k t u
  | isSessionType k t && isSessionType k u = bisim (normalise g) [x] [y]
  | otherwise = False
  where (x, state)     = convertToGNF initial t
        (y, (g, _, _)) = convertToGNF state u
  
checkBinding :: KindEnv -> TypeMap -> Bool -> Constructor -> Type -> Bool
checkBinding k m acc l t = acc && l `Map.member` m && equivalent k (m Map.! l) t

-- -- testing

-- convertTwo :: Type -> Type -> (TypeVar, TypeVar, (Grammar, Visited, Int))
-- convertTwo t u = (x, y, s)
--   where (x, state) = convertToGNF initial t
--         (y, s) = convertToGNF state u

-- alphaKinding = Map.singleton "α" (Kind Session Lin)

-- e1 = equivalent alphaKinding s1 s1
-- e2 = equivalent alphaKinding s1 s2 -- False
-- e3 = equivalent alphaKinding s1 s3 -- False
-- e4 = equivalent alphaKinding s3 s3
-- e5 = equivalent alphaKinding s3 s4 -- False
-- e6 = equivalent alphaKinding s1 s5 -- False
-- e7 = equivalent alphaKinding s4 s5 -- False
-- e8 = equivalent alphaKinding s5 s6 -- False
-- e9 = equivalent alphaKinding s9 s9
-- e10 = equivalent alphaKinding treeSend treeSend
-- e11 = equivalent alphaKinding s21 s22
-- e12 = equivalent alphaKinding s1 s23
-- e13 = equivalent alphaKinding s24 s24
-- e14 = equivalent alphaKinding s24 s25
-- e15 = equivalent alphaKinding s26 s27

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
