{-# LANGUAGE NoMonadFailDesugaring #-}
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

module Validation.TypeToGrammar
( convertToGrammar
--, TransState
--, toGrammar
--, freshVar
--, insertProduction
--, getGrammar
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Validation.Grammar
import           Syntax.Types
import           Syntax.Kinds -- for testing

-- The state of the translation to grammars

type Visited = Map.Map Type TypeVar

type TState = (Productions, Visited, Int)

type TransState = State TState

-- State manipulating functions

initial :: TState
initial = (Map.empty, Map.empty, 0)

freshVar :: TransState String
freshVar = do
  (_, _, n) <- get
  modify (\(p, v, n) -> (p, v, n + 1))
  return $ "_x" ++ show n

lookupVisited :: Type -> TransState (Maybe TypeVar)
lookupVisited t = do
  (_, v, _) <- get
  return $ Map.lookup t v

insertVisited :: Type -> TypeVar -> TransState ()
insertVisited t x =
  modify $ \(p, v, n) -> (p, Map.insert t x v, n)

getGrammar :: TransState Productions
getGrammar = do
  (p, _, _) <- get
  return p

getTransitions :: TypeVar -> TransState Transitions
getTransitions x = do
  p <- getGrammar
  return $ p Map.! x

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \(p, v, n) -> (insertProduction p x l w, v, n)

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \(p, v, n) -> (Map.insert x m p, v, n)

removeProductions :: TypeVar -> TransState ()
removeProductions x =
  modify $ \(p, v, n) -> (Map.delete x p, v, n)

member :: TypeVar -> TransState Bool
member x = do
  p <- getGrammar
  return $ Map.member x p

insertGrammar :: TypeVar -> Transitions -> TransState ()
insertGrammar x m = insertGrammar' x (Map.assocs m)

insertGrammar' :: TypeVar -> [(Label, [TypeVar])] -> TransState ()
insertGrammar' x [] = return ()
insertGrammar' x ((l, w):as) = do
  addProduction x l w
  insertGrammar' x as

replaceInGrammar :: [TypeVar] -> TypeVar -> TransState ()
replaceInGrammar w x =
  modify (\(p, v, n) -> (Map.map (Map.map (replace w x)) p, v, n))

replace :: [TypeVar] -> TypeVar -> [TypeVar] -> [TypeVar]
replace _ _ [] = []
replace w x (y:ys)
  | x == y    = w ++ (replace w x ys)
  | otherwise = y : (replace w x ys)

-- Conversion to context-free grammars

convertToGrammar :: [Type] -> (Productions, [TypeVar])
convertToGrammar ts = (p, w)
  where (w, (p, _, _)) = runState (toGrammar00 ts) initial

toGrammar00 :: [Type] -> TransState [TypeVar]
toGrammar00 []     = return []
toGrammar00 (t:ts) = do
  [x] <- toGrammar0 t
  xs <- toGrammar00 ts
  return (x:xs)
{-
convertToGrammar :: TState -> Type -> (TypeVar, TState)
convertToGrammar state t = (x, state')
  where ([x], state') = runState (toGrammar0 t) state
-}
toGrammar0 :: Type -> TransState [TypeVar]
toGrammar0 t = do
  xs <- toGrammar t
  y <- freshVar
  addProduction y (MessageLabel In UnitType) xs
  return [y]

toGrammar :: Type -> TransState [TypeVar]
toGrammar t = do
  maybe <- lookupVisited t
  case maybe of
    Nothing -> toGrammar' t
    Just x ->  return [x]

toGrammar' :: Type -> TransState [TypeVar]
toGrammar' Skip =
  return []
toGrammar' (Message p b) = do
  y <- freshVar
  addProduction y (MessageLabel p b) []
  return [y]
toGrammar' (Var a) = do -- This is a free variable
  y <- freshVar
  addProduction y (VarLabel a) []
  return [y]
{-
toGrammar' (Semi (Choice p m) u) = do
  xs <- toGrammar (Choice p m)
  ys <- toGrammar u
  if null xs
  then return ys
--  else if null ys
--  then return xs
  --else if t == (Rec b t)
  --then return $ xs ++ ys
  else do
    let (x:xs') = xs
    b <- member x
    if b then do
      m <- getTransitions x
      insertGrammar x (Map.map (++ xs' ++ ys) m)
      return [x]
    else
      return $ xs ++ ys -- E.g., rec x. !Int;(x;x)
-}
toGrammar' (Semi t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar' (Rec Bind{var=x} t) = do
  y <- freshVar
  insertVisited (Var y) y
  let u = subs (Var y) x t -- On the fly alpha conversion
--  (z:zs) <- toGrammar u
  zs <- toGrammar u
  if null zs
    then return []
  else do
    m <- getTransitions $ head zs
    addProductions y (Map.map (++ tail zs) m)
    return [y]
toGrammar' (Choice c m) = do
  y <- freshVar
  assocsToGrammar y c (Map.assocs m)
  return [y]

assocsToGrammar :: TypeVar -> ChoiceView -> [(Constructor, Type)] -> TransState ()
assocsToGrammar _ _ [] = return ()
assocsToGrammar y c ((l, t):as) = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel c l) xs
  assocsToGrammar y c as

-- testing

buildGrammar :: Type -> Grammar
buildGrammar t = evalState (generateGrammar t) initial

generateGrammar :: Type -> TransState Grammar
generateGrammar t = do
  w <- toGrammar t
  y <- freshVar
  addProduction y (MessageLabel In UnitType) w
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
t28 = buildGrammar s28
s29 = Semi s5 (Message In IntType)
t29 = buildGrammar s29
s30 = Rec yBind s29
t30 = buildGrammar s29

