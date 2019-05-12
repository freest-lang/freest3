
{- |
Module      :  Equivalence.TypeToGrammar
Description :  Conversion from types to grammars
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module builds the initial monadic state, and converts the session types
given as parameter to context-free grammars
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Equivalence.TypeToGrammar
( convertToGrammar
) where

import           Equivalence.Grammar
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace

-- Conversion to context-free grammars

convertToGrammar :: TypeEnv -> [Type] -> Grammar
convertToGrammar tEnv ts = Grammar xs (productions s)
  where (xs, s) = runState (mapM typeToGrammar ts) (initial tEnv)

typeToGrammar :: Type -> TransState TypeVar
typeToGrammar t = do
  xs <- toGrammar t
  y <- freshVar
  addProduction y (MessageLabel Out UnitType) xs
  return y

toGrammar :: Type -> TransState [TypeVar]
-- Session types
toGrammar (Skip _) =
  return []
toGrammar (Semi _ t u) = do
  xs <- toGrammar t
  ys <- toGrammar u
  return $ xs ++ ys
toGrammar (Message _ p b) = do
  y <- addBasicProd (MessageLabel p b)
  return [y]
toGrammar (Choice _ p m) = do
  y <- freshVar
  mapM_ (assocToGrammar y p) (Map.assocs m) -- TODO: avoid Map.assocs; run map through the monad
  return [y]
-- Functional or session (session in this case)
toGrammar (TypeVar _ x) = do
  b <- memberVisited x
  if b
  then    -- This is a recursion variable
    return [x]
  else do -- This is a polymorphic variable
    y <- addBasicProd (VarLabel x)
    return [y]
toGrammar (Rec _ (TypeVarBind _ x _) t) = do
  insertVisited x
  toGrammar t >>= \case
    []     -> return []
    (z:zs) ->
      getTransitions z >>= \case
        Just m -> do
          addProductions x (Map.map (++ zs) m)
          return [x]
        Nothing -> do
          b <- memberVisited z
          if b && z/=x then -- case in which the productions for z are not yet completed
            return (z:zs)
          else
            return []
  -- Type operators
toGrammar (Dualof p (TypeName _ x)) = do
  insertVisited x
  (k, TypeScheme _ [] t) <- getFromVEnv x
  trace ("Type " ++ show x ++ " = " ++ show t)
    toGrammar (Dualof p t)
toGrammar (Dualof _ t) = toGrammar (dual t)
toGrammar (TypeName _ x) = do
  b <- memberVisited x
  if b
  then    -- We have visited this type name before
    return [x]
  else do -- This is the first visit
    insertVisited x
    (k, TypeScheme _ [] t) <- getFromVEnv x
    toGrammar t -- (Rec p (TypeVarBind q x k) t)
  -- Should not happen
toGrammar t = error ("toGrammar: " ++ show t)

assocToGrammar :: TypeVar -> Polarity -> (ProgVar, Type) -> TransState ()
assocToGrammar y p (x, t) = do
  xs <- toGrammar t
  addProduction y (ChoiceLabel p x) xs

-- The state of the translation to grammars

type Visited = Set.Set TypeVar

data TState = TState {
  productions :: Productions
, visited     :: Visited
, nextIndex   :: Int
, typeEnv     :: TypeEnv
}

type TransState = State TState

-- State manipulating functions

initial :: TypeEnv -> TState
initial tEnv = TState {
  productions = Map.empty
, visited     = Set.empty
, nextIndex   = 1
, typeEnv     = tEnv
}

freshVar :: TransState TypeVar
freshVar = do
  s <- get
  let n = nextIndex s
  modify (\s -> s {nextIndex = n + 1})
  return $ mkVar defaultPos ("#X" ++ show n)

memberVisited :: TypeVar -> TransState Bool
memberVisited x = do
  s <- get
  return $ x `Set.member` (visited s)

insertVisited :: TypeVar -> TransState ()
insertVisited x =
  modify $ \s -> s {visited = Set.insert x (visited s)}

getTransitions :: TypeVar -> TransState (Maybe Transitions)
getTransitions x = do
  s <- get
  return $ (productions s) Map.!? x

addProductions :: TypeVar -> Transitions -> TransState ()
addProductions x m =
  modify $ \s -> s {productions = Map.insert x m (productions s)}

addProduction :: TypeVar -> Label -> [TypeVar] -> TransState ()
addProduction x l w =
  modify $ \s -> s {productions = insertProduction (productions s) x l w}

-- Add or update production from a (basic) non-terminal; the
-- productions may already contain transitions for the given
-- nonterminal (hence the insertWith and union)
addBasicProd :: Label -> TransState TypeVar
addBasicProd l = do
  s <- get
  case Map.foldrWithKey fold Nothing (productions s) of
    Nothing -> do
      y <- freshVar
      addProduction y l []
      return y
    Just p ->
      return p
  where fold x m acc = if l `Map.member` m && null (m Map.! l) then Just x else acc
  -- y <- freshVar
  -- addProduction y l []
  -- return y

getFromVEnv :: TypeVar -> TransState (Kind, TypeScheme)
getFromVEnv x = do
  s <- get
  return $ (typeEnv s) Map.! x

-- Some tests
{-
p = (0,0)
s1 = Message p Out CharType
t1 = convertToGrammar [s1]
s2 = TypeVar p "α"
t2 = convertToGrammar [s2]
s3 = Semi p (Message p Out IntType) (Message p In BoolType)
t3 = convertToGrammar [s3]
s4 = Semi p s3 s1
t4 = convertToGrammar [s4]
s5 = Choice p External (Map.fromList
  [("Leaf", Skip p),
   ("Node", s1)])
t5 = convertToGrammar [s5]
s6 = Choice p External (Map.fromList
  [("Leaf", Skip p),
   ("Node", s3)])
t6 = convertToGrammar [s6]
-- yBind = Bind "y" p (Kind {prekind = Session, multiplicity = Lin})
yBind = "y"
treeSend = Rec p yBind (Choice p External (Map.fromList
  [("Leaf",Skip p),
   ("Node", Semi p (Message p Out IntType) (Semi p (TypeVar p "y") (TypeVar p "y")))]))
t7 = convertToGrammar [treeSend]
t8 = convertToGrammar [Semi p treeSend (TypeVar p "α")]
s9 = Rec p yBind (Semi p s1 (TypeVar p "y"))
t9 = convertToGrammar [s9]
s10 = Semi p s4 (Semi p (Semi p s3 s1) s4)
t10 = convertToGrammar [s10]
s11 = Semi p (Rec p yBind (Semi p treeSend (TypeVar p "y"))) treeSend
t11 = convertToGrammar [s11]
-- zBind = Bind "z" p (Kind {prekind = Session, multiplicity = Lin})
zBind = "z"
s12 = Semi p (Rec p zBind (Semi p treeSend (TypeVar p "z"))) treeSend
t12 = convertToGrammar [s12]
s13 = Semi p treeSend (Skip p)
t13 = convertToGrammar [s13]
s14 = Semi p (Skip p) treeSend
t14 = convertToGrammar [s14]
s15 = Semi p treeSend treeSend
t15 = convertToGrammar [s15]
treeSend1 = Rec p zBind (Choice p External (Map.fromList
  [("Leaf",Skip p),
   ("Node", Semi p (Message p Out IntType) (Semi p (TypeVar p "z") (TypeVar p "z")))]))
s16 = Semi p treeSend treeSend1
t16 = convertToGrammar [s16]
s17 = Rec p zBind (Semi p s1 (TypeVar p "z"))
t17 = convertToGrammar [s17]
s18 = Rec p zBind (Semi p s1 (Semi p (TypeVar p "z") (TypeVar p "z")))
t18 = convertToGrammar [s18]
s19 = Rec p zBind (Semi p (Semi p s1 (TypeVar p "z")) (TypeVar p "z"))
t19 = convertToGrammar [s19]
s20 = Message p In IntType
s21 = Semi p s1 (Semi p s2 s20)
s22 = Semi p (Semi p s1 s2) s20
s23 = Semi p s1 (Skip p)
s24 = Rec p yBind (Rec p zBind (Semi p (Semi p s1 (TypeVar p "y")) (TypeVar p "z")))
t24 = convertToGrammar [s24]
s25 = Rec p yBind (Rec p zBind (Semi p (Semi p s1 (TypeVar p "z")) (TypeVar p "y")))
t25 = convertToGrammar [s25]
s26 = Semi p (Choice p External (Map.fromList [("Leaf", Skip p)])) (TypeVar p "α")
t26 = convertToGrammar [s26]
s27 = Choice p External (Map.fromList [("Leaf", (TypeVar p "α"))])
t27 = convertToGrammar [s27]
s28 = Rec p yBind (Choice p External (Map.fromList [("Add", Semi p (Semi p (TypeVar p "y") (TypeVar p "y")) (Message p Out IntType)), ("Const", Skip p)]))
t28 = convertToGrammar [s28]
s29 = Semi p s5 (Message p In IntType)
t29 = convertToGrammar [s29]
s30 = Rec p yBind s29
t30 = convertToGrammar [s24,s25,s26,s27,s28,s29,s30]
-}
