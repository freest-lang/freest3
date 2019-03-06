
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

module Equivalence.TypeToGrammar
( convertToGrammar
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Syntax.Types
import           Syntax.Kinds -- for testing
import           Equivalence.Grammar

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

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \(p, v, n) -> (Map.insert x m p, v, n)

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \(p, v, n) -> (insertProduction p x l w, v, n)
--  addProductions x (Map.singleton l w) -- does not work; I wonder why

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
toGrammar (Skip _) =
  return []
toGrammar (Message _ p b) = do
  y <- freshVar
  addProduction y (MessageLabel p b) []
  return [y]
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Var _ x) = do
  b <- memberVisited x
  if b
  then    -- This is a recursion variable
    return [x]
  else do -- This is a polymorphic variable
    y <- freshVar
    addProduction y (VarLabel x) []
    return [y]
toGrammar u@(Rec p Bind{var=x} t)
  | isChecked u Set.empty = return []
  | otherwise = do
    y <- freshVar
    insertVisited y
    zs <- toGrammar $ subs (Var p y) x t -- On the fly α-conversion
    -- TODO: use the p in the Bind
    m <- getTransitions $ head zs
    addProductions y (Map.map (++ tail zs) m)
    return [y]
toGrammar (Choice _ c m) = do
  y <- freshVar
  mapM_ (assocToGrammar y c) (Map.assocs m)
  return [y]

assocToGrammar :: TypeVar -> ChoiceView -> (Constructor, Type) -> TransState ()
assocToGrammar y c (l, t) = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel c l) xs

isChecked :: Type -> Visited -> Bool
isChecked (Skip _) _ = True
isChecked (Semi _ s t) v = isChecked s v && isChecked t v
isChecked (Rec _ Bind{var=x} t) v = isChecked t (Set.insert x v)
isChecked (Var _ x) v = Set.member x v -- Only bound variables are checked
isChecked _ _ = False

-- Some tests

s1 = Message (0,0) Out CharType
t1 = convertToGrammar [s1]
s2 = Var (0,0) "α"
t2 = convertToGrammar [s2]
s3 = Semi (0,0) (Message (0,0) Out IntType) (Message (0,0) In BoolType)
t3 = convertToGrammar [s3]
s4 = Semi (0,0) s3 s1
t4 = convertToGrammar [s4]
s5 = Choice (0,0) External (Map.fromList
  [("Leaf", Skip (0,0)),
   ("Node", s1)])
t5 = convertToGrammar [s5]
s6 = Choice (0,0) External (Map.fromList
  [("Leaf", Skip (0,0)),
   ("Node", s3)])
t6 = convertToGrammar [s6]
yBind = Bind "y" (0,0) (Kind {prekind = Session, multiplicity = Lin})
treeSend = Rec (0,0) yBind (Choice (0,0) External (Map.fromList
  [("Leaf",Skip (0,0)),
   ("Node", Semi (0,0) (Message (0,0) Out IntType) (Semi (0,0) (Var (0,0) "y") (Var (0,0) "y")))]))
t7 = convertToGrammar [treeSend]
t8 = convertToGrammar [Semi (0,0) treeSend (Var (0,0) "α")]
s9 = Rec (0,0) yBind (Semi (0,0) s1 (Var (0,0) "y"))
t9 = convertToGrammar [s9]
s10 = Semi (0,0) s4 (Semi (0,0) (Semi (0,0) s3 s1) s4)
t10 = convertToGrammar [s10]
s11 = Semi (0,0) (Rec (0,0) yBind (Semi (0,0) treeSend (Var (0,0) "y"))) treeSend
t11 = convertToGrammar [s11]
zBind = Bind "z" (0,0) (Kind {prekind = Session, multiplicity = Lin})
s12 = Semi (0,0) (Rec (0,0) zBind (Semi (0,0) treeSend (Var (0,0) "z"))) treeSend
t12 = convertToGrammar [s12]
s13 = Semi (0,0) treeSend (Skip (0,0))
t13 = convertToGrammar [s13]
s14 = Semi (0,0) (Skip (0,0)) treeSend
t14 = convertToGrammar [s14]
s15 = Semi (0,0) treeSend treeSend
t15 = convertToGrammar [s15]
treeSend1 = Rec (0,0) zBind (Choice (0,0) External (Map.fromList
  [("Leaf",Skip (0,0)),
   ("Node", Semi (0,0) (Message (0,0) Out IntType) (Semi (0,0) (Var (0,0) "z") (Var (0,0) "z")))]))
s16 = Semi (0,0) treeSend treeSend1
t16 = convertToGrammar [s16]
s17 = Rec (0,0) zBind (Semi (0,0) s1 (Var (0,0) "z"))
t17 = convertToGrammar [s17]
s18 = Rec (0,0) zBind (Semi (0,0) s1 (Semi (0,0) (Var (0,0) "z") (Var (0,0) "z")))
t18 = convertToGrammar [s18]
s19 = Rec (0,0) zBind (Semi (0,0) (Semi (0,0) s1 (Var (0,0) "z")) (Var (0,0) "z"))
t19 = convertToGrammar [s19]
s20 = Message (0,0) In IntType
s21 = Semi (0,0) s1 (Semi (0,0) s2 s20)
s22 = Semi (0,0) (Semi (0,0) s1 s2) s20
s23 = Semi (0,0) s1 (Skip (0,0))
s24 = Rec (0,0) yBind (Rec (0,0) zBind (Semi (0,0) (Semi (0,0) s1 (Var (0,0) "y")) (Var (0,0) "z")))
t24 = convertToGrammar [s24]
s25 = Rec (0,0) yBind (Rec (0,0) zBind (Semi (0,0) (Semi (0,0) s1 (Var (0,0) "z")) (Var (0,0) "y")))
t25 = convertToGrammar [s25]
s26 = Semi (0,0) (Choice (0,0) External (Map.fromList [("Leaf", Skip (0,0))])) (Var (0,0) "α")
t26 = convertToGrammar [s26]
s27 = Choice (0,0) External (Map.fromList [("Leaf", (Var (0,0) "α"))])
t27 = convertToGrammar [s27]
s28 = Rec (0,0) yBind (Choice (0,0) External (Map.fromList [("Add", Semi (0,0) (Semi (0,0) (Var (0,0) "y") (Var (0,0) "y")) (Message (0,0) Out IntType)), ("Const", Skip (0,0))]))
t28 = convertToGrammar [s28]
s29 = Semi (0,0) s5 (Message (0,0) In IntType)
t29 = convertToGrammar [s29]
s30 = Rec (0,0) yBind s29
t30 = convertToGrammar [s24,s25,s26,s27,s28,s29,s30]
