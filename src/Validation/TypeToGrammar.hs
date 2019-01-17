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
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Syntax.Types
import           Syntax.Kinds -- for testing
import           Validation.Grammar

-- The state of the translation to grammars

type Visited = Set.Set TypeVar

type TState = (Productions, Visited, Int)

type TransState = State TState

-- State manipulating functions

initial :: TState
initial = (Map.empty, Set.empty, 1)

freshVar :: TransState String
freshVar = do
  (p, v, n) <- get
  put (p, v, n + 1)
  return $ "_X" ++ show n

memberVisited :: TypeVar -> TransState Bool
memberVisited t = do
  (_, v, _) <- get
  return $ Set.member t v

insertVisited :: TypeVar -> TransState ()
insertVisited x =
  modify $ \(p, v, n) -> (p, Set.insert x v, n)

getTransitions :: TypeVar -> TransState Transitions
getTransitions x = do
  (p, _, _) <- get
  return $ p Map.! x

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \(p, v, n) -> (insertProduction p x l w, v, n)

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \(p, v, n) -> (Map.insert x m p, v, n)

-- Conversion to context-free grammars

convertToGrammar :: [Type] -> Grammar
convertToGrammar ts = Grammar xs p
  where (xs, (p, _, _)) = runState (mapM typeToGrammar ts) initial

typeToGrammar :: Type -> TransState TypeVar
typeToGrammar t = do
  xs <- toGrammar t
  y <- freshVar
  addProduction y (MessageLabel In UnitType) xs
  return y

toGrammar :: Type -> TransState [TypeVar]
toGrammar Skip =
  return []
toGrammar (Message p b) = do
  y <- freshVar
  addProduction y (MessageLabel p b) []
  return [y]
toGrammar (Semi t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Var a) = do
  b <- memberVisited a
  if b
  then    -- This is a recursion variable
    return [a]
  else do -- This is a free variable
    y <- freshVar
    addProduction y (VarLabel a) []
    return [y]
toGrammar (Rec Bind{var=x} t) = do
  y <- freshVar
  insertVisited y
  zs <- toGrammar $ subs (Var y) x t -- On the fly alpha conversion
  if null zs
    then return []
  else do
    m <- getTransitions $ head zs
    addProductions y (Map.map (++ tail zs) m)
    return [y]
toGrammar (Choice c m) = do
  y <- freshVar
  mapM_ (assocToGrammar y c) (Map.assocs m)
  return [y]

assocToGrammar :: TypeVar -> ChoiceView -> (Constructor, Type) -> TransState ()
assocToGrammar y c (l, t) = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel c l) xs

-- Some tests

s1 = Message Out CharType
t1 = convertToGrammar [s1]
s2 = Var "α"
t2 = convertToGrammar [s2]
s3 = Semi (Message Out IntType) (Message In BoolType)
t3 = convertToGrammar [s3]
s4 = Semi s3 s1
t4 = convertToGrammar [s4]
s5 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s1)])
t5 = convertToGrammar [s5]
s6 = Choice External (Map.fromList
  [("Leaf", Skip),
   ("Node", s3)])
t6 = convertToGrammar [s6]
yBind = Bind "y" (Kind {prekind = Session, multiplicity = Lin})
treeSend = Rec yBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "y") (Var "y")))]))
t7 = convertToGrammar [treeSend]
t8 = convertToGrammar [Semi treeSend (Var "α")]
s9 = Rec yBind (Semi s1 (Var "y"))
t9 = convertToGrammar [s9]
s10 = Semi s4 (Semi (Semi s3 s1) s4)
t10 = convertToGrammar [s10]
s11 = Semi (Rec yBind (Semi treeSend (Var "y"))) treeSend
t11 = convertToGrammar [s11]
zBind = Bind "z" (Kind {prekind = Session, multiplicity = Lin})
s12 = Semi (Rec zBind (Semi treeSend (Var "z"))) treeSend
t12 = convertToGrammar [s12]
s13 = Semi treeSend Skip
t13 = convertToGrammar [s13]
s14 = Semi Skip treeSend
t14 = convertToGrammar [s14]
s15 = Semi treeSend treeSend
t15 = convertToGrammar [s15]
treeSend1 = Rec zBind (Choice External (Map.fromList
  [("Leaf",Skip),
   ("Node", Semi (Message Out IntType) (Semi (Var "z") (Var "z")))]))
s16 = Semi treeSend treeSend1
t16 = convertToGrammar [s16]
s17 = Rec zBind (Semi s1 (Var "z"))
t17 = convertToGrammar [s17]
s18 = Rec zBind (Semi s1 (Semi (Var "z") (Var "z")))
t18 = convertToGrammar [s18]
s19 = Rec zBind (Semi (Semi s1 (Var "z")) (Var "z"))
t19 = convertToGrammar [s19]
s20 = Message In IntType
s21 = Semi s1 (Semi s2 s20)
s22 = Semi (Semi s1 s2) s20
s23 = Semi s1 Skip
s24 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "y")) (Var "z")))
t24 = convertToGrammar [s24]
s25 = Rec yBind (Rec zBind (Semi (Semi s1 (Var "z")) (Var "y")))
t25 = convertToGrammar [s25]
s26 = Semi (Choice External (Map.fromList [("Leaf", Skip)])) (Var "α")
t26 = convertToGrammar [s26]
s27 = Choice External (Map.fromList [("Leaf", (Var "α"))])
t27 = convertToGrammar [s27]
s28 = Rec yBind (Choice External (Map.fromList [("Add", Semi (Semi (Var "y") (Var "y")) (Message Out IntType)), ("Const", Skip)]))
t28 = convertToGrammar [s28]
s29 = Semi s5 (Message In IntType)
t29 = convertToGrammar [s29]
s30 = Rec yBind s29
t30 = convertToGrammar [s24,s25,s26,s27,s28,s29,s30]
