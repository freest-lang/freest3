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
import Data.List (intersperse)

-- GREIBACH NORMAL FORMS

type Vars = [TypeVar]

data Label =
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

type Productions = Map.Map TypeVar (Map.Map Label Vars)

data GNF = GNF {start :: TypeVar, productions :: Productions} -- deriving Show

instance Show GNF where
  show g = "start: " ++ start g ++ showProductions (productions g)

showProductions :: Productions -> String
showProductions = Map.foldrWithKey showProds ""

showProds :: TypeVar -> (Map.Map Label Vars) -> String -> String
showProds x m s = s ++ "\n" ++ Map.foldrWithKey (showProd x) "" m

showProd :: TypeVar -> Label -> Vars -> String -> String
showProd x l ys s = s ++ "\n" ++ x ++ " -" ++ show l ++ "-> " ++ concat ys

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

getProductions :: TypeVar -> GNFState (Map.Map Label Vars)
getProductions x = do
  p <- getAllProductions
  return $ p Map.! x

insertProduction :: TypeVar -> Label -> Vars -> GNFState ()
insertProduction x l w =
  modify (\(p, v, n) -> (Map.insertWith Map.union x (Map.singleton l w) p, v, n))

insertProductions :: TypeVar -> (Map.Map Label Vars) -> GNFState ()
insertProductions x m = insertProductions' x (Map.assocs m)

insertProductions' x [] = return ()
insertProductions' x ((l, w):as) = do
  insertProduction x l w
  insertProductions' x as

replaceInProductions :: Vars -> TypeVar -> GNFState ()
replaceInProductions w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: Eq a => [a] -> a -> [a] -> [a]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to GNF

convertToGNF :: Type -> GNF
-- Assume: t is a session type different from Skip
convertToGNF t = evalState (generateGNF t) initial

generateGNF :: Type -> GNFState GNF
generateGNF t = do
  (y:ys) <- toGNF t
  m <- getProductions y
  insertProductions y (Map.map (++ ys) m)
  p <- getAllProductions
  return $ GNF {start = y, productions = p}

toGNF :: Type -> GNFState Vars
toGNF t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGNF' t
    Just x ->  return [x]

toGNF' :: Type -> GNFState Vars
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
  return $ xs ++ ys
toGNF' (Choice p m) = do
  y <- freshVar
  assocsToGNF y p (Map.assocs m)
  return [y]
toGNF' (Rec b t) = do
  y <- freshVar
  let u = rename (Rec b t) y
  insertVisited u y
  (z:zs) <- toGNF (unfold u)
  m <- getProductions z
  insertProductions y (Map.map (++ replace [z] y zs) m)
  replaceInProductions (z:zs) y
  return [z]

assocsToGNF :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> GNFState ()
assocsToGNF _ _ [] = return ()
assocsToGNF y p ((l, t):as) = do
  w <- toGNF t
  insertProduction y (ChoiceLabel p l) w  
  assocsToGNF y p as

-- tests

s1 = Message Out CharType
t1 = convertToGNF s1
t2 = convertToGNF $ (Var "alpha")
s3 = Semi (Message Out IntType) (Message In BoolType)
t3 = convertToGNF s3
s4 = Semi s3 s1
t4 = convertToGNF s4
t5 = convertToGNF $ Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s1)])
t6 = convertToGNF $ Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s3)])
yBind = Bind "y" (Kind {prekind = Session, multiplicity = Lin})
treeSend = Rec yBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
t7 = convertToGNF treeSend
t8 = convertToGNF $ Semi treeSend (Var "alpha")
s9 = Rec yBind (Semi s1 (Var "y"))
t9 = convertToGNF s9
s10 = Semi s4 (Semi (Semi s3 s1) s4)
t10 = convertToGNF s10
s11 = Semi (Rec yBind (Semi treeSend (Var "y"))) treeSend
t11 = convertToGNF s11
zBind = Bind "z" (Kind {prekind = Session, multiplicity = Lin})
s12 = Semi (Rec zBind (Semi treeSend (Var "z"))) treeSend
t12 = convertToGNF s12
s13 = Semi treeSend Skip
t13 = convertToGNF s13
s14 = Semi Skip treeSend
t14 = convertToGNF s14
s15 = Semi treeSend treeSend
t15 = convertToGNF s15
treeSend1 = Rec zBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "z") (Var "z")))]))
s16 = Semi treeSend treeSend1
t16 = convertToGNF s16


-- TYPE EQUIVALENCE

equivalent :: KindEnv -> Type -> Type -> Bool
equivalent _ _ _ = True

{-
bisim :: Word -> Word -> GNF -> Bool
bisim w1 w2 = check [simplify (Set.singleton (w1, w2))]

check :: [Node] -> GNF -> Bool
check ns gnf =
  Set.isempty ns || expandNode (head ns)

-}



unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec (Bind x k) t) = subs (Rec (Bind x k) t) x t

rename :: Type -> TypeVar -> Type
-- Assumes parameter is a Rec type
rename (Rec (Bind x k) t) y = Rec (Bind y k) (subs (Var y) x t)

subs :: Type -> TypeVar -> Type -> Type
subs t y (Var x)
    | x == y              = t
    | otherwise           = Var x
subs t y (Semi t1 t2)     = Semi (subs t y t1) (subs t y t2)
subs t y (PairType t1 t2) = PairType (subs t y t1) (subs t y t2)
-- subs t2 y (Forall x k t1)
--     | x == y                = Forall x k t1
--     | otherwise             = Forall x k (subs t2 y t1)
-- Assume y /= x 
subs t2 y (Rec (Bind x k) t1)
    | x == y              = Rec (Bind x k) t1
    | otherwise           = Rec (Bind x k) (subs t2 y t1)
subs t y (Choice v m)     = Choice v (Map.map(subs t y) m)
subs t y (Fun m t1 t2)    = Fun m (subs t y t1) (subs t y t2)
subs _ _ t                = t
-- subs _ _ Skip               = Skip
-- subs _ _ (In b)             = In b
-- subs _ _ (Out b)            = Out b
