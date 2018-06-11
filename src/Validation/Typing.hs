module Validation.Typing (
  --   typeCheck
  -- , TCheckM
  -- , canonical -- TODO: Remove
  -- , unfold -- TODO: Remove
) where

-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import           Syntax.Kinds
-- 
-- import           Parse.TypeParser -- TODO: Remove?

import Control.Monad.State
import Syntax.Terms
import Syntax.Types
import Validation.Kinding
import Validation.TypeEquivalence
import Validation.TypingState

-- TODO remove
import PreludeLoader
import qualified Data.Map.Strict as Map
import Syntax.Kinds

checkExp :: Expression -> TypingState TypeScheme
-- Basic expressions
checkExp (Unit _)         = return $ TypeScheme [] (Basic UnitType)
checkExp (Integer _ _)    = return $ TypeScheme [] (Basic IntType)
checkExp (Character _ _)  = return $ TypeScheme [] (Basic CharType)
checkExp (Boolean _ _)    = return $ TypeScheme [] (Basic BoolType)

-- Variables
checkExp (Variable p x) = checkVar p x
  
checkExp (UnLet _ x e1 e2) = do
  t1 <- checkExp e1
  addToVEnv x t1
  checkExp e2
  
-- Applications
checkExp (App p e1 e2) = do
  t <- checkExp e1
  (u1, u2) <- extractFun p t
  checkAgainst p e2 u1
  return $ TypeScheme [] u2

checkExp (TypeApp p e t) = do
  t1 <- checkExp e  
  --  checkAgainstKind
  (binds, t1) <- extractScheme p t1
  let sub = foldr (\(t', b) acc -> subs t' (var b) acc) t1 (zip t binds)
  -- TODO: isWellKinded sub???
  return $ TypeScheme [] sub

-- Conditional

checkExp (Conditional p e1 e2 e3) = do
  checkAgainst p e1 (Basic BoolType)
  venv2 <- getVarEnv
  (TypeScheme _ t2) <- checkExp e2  
  venv3 <- getVarEnv
  setVEnv venv2
  checkAgainst p e3 t2
  venv4 <- getVarEnv
  kenv <- getKindEnv
  checkEquivEnvs kenv venv3 venv4
  setVEnv venv2
  return $ TypeScheme [] t2

 -- Pairs

checkExp (Pair p e1 e2) = do
 (TypeScheme _ t1) <- checkExp e1 
 (TypeScheme _ t2) <- checkExp e2 
 return $ TypeScheme [] (PairType t1 t2)

checkExp (BinLet p x y e1 e2) = do
  t1 <- checkExp e1
  (u1,u2) <- extractPair p t1
  addToVEnv x (TypeScheme [] u1)
  addToVEnv y (TypeScheme [] u2)
  u <- checkExp e2 
  -- TODO: remove x and y from env ??
  kenv <- getKindEnv
  removeLinVar kenv x (TypeScheme [] u1)
  removeLinVar kenv y (TypeScheme [] u2)
  --
  return u

-- Session types

checkExp (New p t) = do
  u <- checkAgainstKind t (Kind Session Lin)
  m <- extractChoiceMap u
  Map.foldrWithKey (\c t _ -> addToVEnv c (TypeScheme [] t)) (return ()) m
  return $ TypeScheme [] (PairType u (dual u))

checkExp (Send p e1 e2) = do
  t1 <- checkExp e1
  b1 <- extractBasic p t1
  t2 <- checkExp e2
  (b2, u) <- extractOutput p t2
  checkEquivBasics p b1 b2
  return $ TypeScheme [] u

checkExp (Receive p e) = do
  t <- checkExp e
  (b, t1) <- extractInput p t
  return $ TypeScheme [] (PairType (Basic b) t1)

-- Branching
--   let venv3 = checkInternalToVenv venv2 t' ??
checkExp (Select p c e) = do
  t <- checkExp e
  choice <- extractInChoice p t
  m <- extractChoiceMap choice
  u <- extractConstructor p c m
  return $ TypeScheme [] u



  
-- Functions to deal with environments

checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TypingState ()
checkEquivEnvs kenv venv1 venv2 -- = return ()
  | equivalentEnvs kenv venv1 venv2  = return ()
  | otherwise = addError ("Expecting environment " ++ show venv1 ++
                       " to be equivalent to environment " ++ show venv2)

equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
equivalentEnvs kenv venv1 venv2 =
  let venv3 = Map.filter f1 venv1
      venv4 = Map.filter f1 venv2 in
  Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
  where
    f (TypeScheme _ t) (TypeScheme _ u) = equivalent kenv t u
    f1 (TypeScheme _ t) = lin kenv t

checkEquivBasics :: Pos -> BasicType -> BasicType -> TypingState ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise = addError (show p ++ ": Expecting basic type " ++ (show b1) ++
                      " to be equivalent to basic type " ++ (show b2))

-- The Extract Functions

extractFun :: Pos -> TypeScheme -> TypingState (Type, Type)
extractFun _ (TypeScheme [] (Fun _ t u)) = return (t, u)
extractFun p t                           = do
  addError (show p ++  ": Expecting a function type; found " ++ show t)
  return (Basic IntType, Basic IntType)

extractScheme :: Pos -> TypeScheme -> TypingState ([Bind], Type)
extractScheme p (TypeScheme [] t) = do
  addError (show p ++ ": Expecting a type scheme; found " ++ show t)
  return ([], (Basic UnitType))
extractScheme _ (TypeScheme bs t) = return (bs, t)

extractPair :: Pos -> TypeScheme -> TypingState (Type, Type)
extractPair _ (TypeScheme [] (PairType t u)) = return (t, u)
extractPair p t                         = do
  addError (show p ++  ": Expecting a pair type; found " ++ show t)
  return (Basic IntType, Basic IntType)

extractBasic :: Pos -> TypeScheme -> TypingState BasicType
extractBasic _ (TypeScheme [] (Basic t)) = return t
extractBasic p t                         = do
  addError (show p ++  ": Expecting a basic type; found " ++ show t)
  return UnitType

-- !~> 
extractOutput :: Pos -> TypeScheme -> TypingState (BasicType, Type)
extractOutput p (TypeScheme [] (Semi Skip t)) =  extractOutput p (TypeScheme [] t)
extractOutput _ (TypeScheme [] (Semi (Message Out b) t)) = return (b,t)
extractOutput _ (TypeScheme [] (Message Out b)) = return (b, Skip)
extractOutput p (TypeScheme [] (Rec b t)) = extractOutput p (TypeScheme [] (unfold (Rec b t)))
extractOutput p (TypeScheme [] (Semi t u)) = do -- TODO: Wrong?
  (b, t1) <- extractOutput p (TypeScheme [] t)
  return (b, t1 `Semi` u)
extractOutput p t = do
  addError (show p ++  ": Expecting an output type; found " ++ show t)
  return (UnitType, Skip)

extractInput :: Pos -> TypeScheme -> TypingState (BasicType, Type)
extractInput p (TypeScheme [] (Semi Skip t)) =  extractInput p (TypeScheme [] t)
extractInput _ (TypeScheme [] (Semi (Message In b) t)) = return (b,t)
extractInput _ (TypeScheme [] (Message In b)) = return (b, Skip)
extractInput p (TypeScheme [] (Rec b t)) = extractInput p (TypeScheme [] (unfold (Rec b t)))
extractInput p (TypeScheme [] (Semi t u)) = do -- TODO: Wrong?
  (b, t1) <- extractInput p (TypeScheme [] t)
  return (b, t1 `Semi` u)
extractInput p t = do
  addError (show p ++  ": Expecting an input type; found " ++ show t)
  return (UnitType, Skip)

extractInChoice :: Pos -> TypeScheme -> TypingState Type
extractInChoice p (TypeScheme [] (Semi Skip t)) =  extractInChoice p (TypeScheme [] t)
extractInChoice _ (TypeScheme [] (Choice Internal m)) = return $ Choice Internal m
extractInChoice _ (TypeScheme [] (Semi (Choice Internal m) t)) =
  return $ Choice Internal (Map.map (`Semi` t) m)
extractInChoice p (TypeScheme [] (Rec b t)) = extractInChoice p (TypeScheme [] (unfold (Rec b t)))
-- TODO: Finish
-- extractInChoice p (TypeScheme [] (Semi t1 t2)) = do
--   t3 <- extractInChoice p t1
--   return (b, Skip)
extractInChoice p t = do
  addError (show p ++  ": Expecting an internal choice; found " ++ show t)
  return Skip

extractConstructor :: Pos -> Constructor -> TypeMap -> TypingState Type
extractConstructor p c tm =
  if Map.member c tm then
    return $ tm Map.! c 
  else do
    addError (show p ++ ": Constructor '" ++ show c ++ "'not in scope" )
    return (Basic UnitType)

-- Extract Without Errors

extractChoiceMap :: Type -> TypingState TypeMap
extractChoiceMap  (Choice _ m) = return m
extractChoiceMap  (Semi (Choice _ m) t2) = return (Map.map (`Semi` t2) m)
extractChoiceMap  (Semi t1 t2) = do
  m1 <- extractChoiceMap t1
  m2 <- extractChoiceMap t2
  return $ Map.union m1 m2
extractChoiceMap  _ = return Map.empty



-- Checking variables

checkVar :: Pos -> TermVar -> TypingState TypeScheme
checkVar pos x = do
  member <- venvMember x
  if member then do
    t <- getFromVarEnv x
    kenv <- getKindEnv
    removeLinVar kenv x t
    return t
  else do
    addError (show pos ++  ": Variable or data constructor not in scope: " ++ x)
    return $ TypeScheme [] (Basic UnitType)

removeLinVar :: KindEnv -> TermVar -> TypeScheme -> TypingState ()
removeLinVar kenv x (TypeScheme _ t)
  | lin kenv t = removeFromVarEnv x   
  | otherwise  = return ()


-- Type check against type

checkAgainst :: Pos -> Expression -> Type -> TypingState ()
checkAgainst p e t = do
  (TypeScheme _ u) <- checkExp e
  kenv <- getKindEnv
  if (equivalent kenv t u) then
    return ()
  else
    addError (show p ++ ": Expecting type " ++ show t ++ " to be equivalent to type " ++ show u)



-- TESTS

test e = -- runState (checkExp e) initState
  let (r, (_, _, err)) = runState (checkExp e) initState in
    (r, err)

-- runState :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s


e1 = (App (0,0) (App (0,1) (Variable (0,2) "(+)") (Integer (0,3) 2)) (Integer (0,4) 2))
e2 = (App (1,0) (App (1,1) (Variable (1,2) "(+)") (Integer (1,3) 2)) (Boolean (1,4) True))
e3 = (UnLet (2, 0) "x" e1 (Variable (2, 10) "x"))

e4 = (UnLet (2, 0) "x" e1 (Variable (2, 10) "y"))


initState :: (KindEnv, VarEnv, Errors)
initState = (initKindEnv, initVarEnv, [])

initVarEnv =
  let v1 = prelude
      v2 = Map.insert "myId" t3 v1
      v3 = Map.insert "c" (TypeScheme [] cType) v2
      v4 = Map.insert "w" (TypeScheme [] wType) v3
  in v4

cType = read "!();!Int;?Bool;Skip" :: Type
wType = read "Skip;Skip;Skip;+{Plus:Int};Skip" :: Type

initKindEnv =
  Map.foldrWithKey (\x (TypeScheme _ y) acc -> Map.insert x (kindOf y) acc) Map.empty initVarEnv
  

t1 = TypeScheme [] (Fun Lin (Basic IntType) (Basic IntType))
t2 (TypeScheme _ t) kenv = lin kenv t

-- t3
-- type app

t3 = TypeScheme [(Bind {var="a",kind=kindt3})] t3'
t3' = read "a -> a" :: Type
kindt3 = Kind Session Un


-- myId Error
e5 = App (58,6) (Variable (58,6) "myId") (Integer (58,11) 1)
-- myId Ok ...
e6 = App (61,6) (TypeApp (61,6) (Variable (61,6) "myId") [(Basic IntType)]) (Integer (61,17) 1)

-- conditional

e7 = Conditional (64,15) (Boolean (64,15) True) (Integer (64,22) 2) (Integer (64,29) 2)

-- invalid conditional

e8 = Conditional (64,15) (Integer (64,15) 2) (Integer (64,22) 2) (Integer (64,29) 2)
e9 = UnLet (69,7) "y" (Conditional (70,8) (Boolean (70,8) True) (UnLet (71,11) "x" (Integer (71,15) 5) (Integer (71,20) 7)) (UnLet (73,11) "x" (Integer (73,15) 3) (Integer (73,20) 9))) (Variable (74,6) "x")

-- bin let and pairs
-- False
e10 = BinLet (81,15) "x" "y" (Integer (81,22) 1) (Variable (81,27) "x")
-- True
e11 = BinLet (78,7) "x" "y" (Pair (78,15) (Integer (78,15) 1) (Integer (78,17) 2)) (Variable (78,23) "x")

-- new
e12 = New (85,12) (Message Out IntType)
e13 = New (85,12) (Choice Internal (Map.fromList [("A", (Message Out IntType))]))
e14 = New (85,12) (Semi (Choice Internal (Map.fromList [("A", (Message Out IntType))])) (Message In IntType))
e15 = New (85,12) (Semi (Semi (Choice Internal (Map.fromList [("A", (Message Out IntType))])) (Message In IntType)) ((Message In BoolType)))

e16 = New (85,12) (Semi (Choice Internal (Map.fromList [("A", (Message Out IntType))]))
                   (Semi (Message In IntType) (Message In BoolType)))

-- send receive
-- add c
e17 = UnLet (88,7) "c1" (Send (88,17) (Integer (88,17) 5) (Variable (88,19) "c")) (BinLet (89,7) "b" "c2" (Receive (89,23) (Variable (89,23) "c1")) (Unit (90,6)))

-- error on send
e18 = UnLet (88,7) "c1" (Send (88,17) (Boolean (88,17) True) (Variable (88,19) "c")) (BinLet (89,7) "b" "c2" (Receive (89,23) (Variable (89,23) "c1")) (Unit (90,6)))

-- error on receive
e19 = UnLet (88,7) "c1" (Send (88,17) (Integer (88,17) 5) (Variable (88,19) "c")) (BinLet (89,7) "b" "c2" (Receive (89,23) (Variable (89,23) "c13")) (Unit (90,6)))

-- error on receive2
-- change c to Skip in the begining
e20 = UnLet (88,7) "c1" (Send (88,17) (Integer (88,17) 5) (Variable (88,19) "c")) (BinLet (89,7) "b" "c2" (Receive (89,23) (Variable (89,23) "c1")) (Unit (90,6)))


-- SELECT
-- 
e21 = Select (96,19) "Plus" (Variable (96,24) "w")
-- Error: C not int scope: "Plusa"
e21 = Select (96,19) "Plusa" (Variable (96,24) "w")

