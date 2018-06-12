module Validation.Typing (
     typeCheck  
) where

import Control.Monad.State
import Syntax.Terms
import Syntax.Types
import Validation.Kinding
import Validation.TypeEquivalence
import Validation.TypingState
import qualified Data.Set as Set

-- TODO remove
import PreludeLoader
import qualified Data.Map.Strict as Map
import Syntax.Kinds


typeCheck ::  ExpEnv -> ConstructorEnv -> TypingState ()
typeCheck eenv cenv = do
  -- 1 - Data declaration
  checkDataDecl cenv
  
  -- 2 - Function type declaration
  pure $ Map.mapWithKey (\fun (a, e) -> checkFunTypeDecl fun) eenv

  -- 3 - Function declaration  
  checkVar (0,0) "start"

  venv1 <- getVarEnv
  let venv2 = Map.union venv1 cenv
  setVEnv venv2


  let a = Map.mapWithKey (\fun (a, e) -> checkFD fun a e) eenv
 -- let a =
  -- Map.foldrWithKey (\fun (a, e) _ -> checkFD fun a e) (return ()) eenv
  -- tell $ Map.foldl (\acc v -> acc ++ execWriter v) [] a
--  let err =
  s <- get
  addErrorList $ Map.foldl (\acc v -> acc ++ errors s v) [] a
  -- mapM (addError) err
  
  return ()

errors :: (KindEnv, VarEnv, Errors) -> TypingState () -> [String]
errors is s =
  let (_,_,err) = execState s is in
    err

checkFD ::  TermVar -> Params -> Expression -> TypingState ()
checkFD fname args exp = do
  checkExpEnv (0,0) fname args
  venv <- getVarEnv
  let (TypeScheme _ lt) = last $ toList $ venv Map.! fname
  checkAgainst (0,0) exp lt

--  checkVEnvUn kenv venv2
  return ()

checkExpEnv :: Pos -> TermVar -> Params -> TypingState ()
checkExpEnv p fun params = do
  checkParam fun params
  t <- checkVar p fun
  parameters <- addToEnv p fun params (init (toList t))  
--  parameters <- addToEnv fun params (init (toList (venv Map.! fun)))
  venv <- getVarEnv
  -- Map.insert arg t acc
  foldM (\acc (arg, t) -> addToVEnv arg t) () parameters
  return ()

addToEnv :: Pos -> TypeVar -> Params -> [TypeScheme] -> TypingState [(TypeVar, TypeScheme)]
addToEnv p c ps ts 
  | length ps == length ts = return $ zip ps ts--(map canonical ts)
  | length ps > length ts = do
      addError ("Function or constructor " ++ show c ++ " is applied to too many arguments")
      return []
  | length ps < length ts = do
      addError ("Function or constructor " ++ show c ++ " is applied to too few arguments")
      return []

checkParam :: TermVar -> Params -> TypingState ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     addError ("Conflicting definitions for " ++ fun ++
           "'\n" ++ "In an equation for '" ++ fun ++ "'")
     return ()


checkDataDecl :: ConstructorEnv -> TypingState ()
checkDataDecl cenv = do
  kenv <- getKindEnv
  Map.foldl (\_ k -> checkFunctionalKind k) (return ()) kenv
  Map.foldl (\_ t -> checkKinding kenv t) (return ()) cenv 

checkFunctionalKind :: Kind -> TypingState ()
checkFunctionalKind k
  | k >= (Kind Functional Un) = return ()
  | otherwise = addError ("Expecting a functional (TU or TL) type; found a " ++ (show k) ++ " type.")

checkFunTypeDecl :: TermVar -> TypingState ()
checkFunTypeDecl fname = do  
  t <- checkVar (0,0) fname
  kenv <- getKindEnv
  checkKinding kenv t
  return ()

checkKinding :: KindEnv -> TypeScheme -> TypingState ()
checkKinding kenv (TypeScheme _ t) = kinding t >> return ()


-- Typing rules for expressions

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
  -- was:
  -- kenv <- getKindEnv
  -- removeLinVar kenv x (TypeScheme [] u1)
  -- removeLinVar kenv y (TypeScheme [] u2)
  --
  removeFromVarEnv x
  removeFromVarEnv y
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

checkExp (Match p e cm) = do
  t <- checkExp e
--  m <- extractExtChoice p t
  let (c, (x, e1)) = Map.elemAt 0 cm
  t2 <- extractExtChoice p t c  
  
  addToVEnv x (TypeScheme [] t2)
  u <- checkExp e1

  Map.foldrWithKey (\k (v1,v2) _ -> checkMap p t u k ([v1], v2) extractExtChoice) (return ()) (Map.delete c cm)

  return u
--  return $ TypeScheme [] (Basic UnitType)

  -- let (c, t1) = Map.elemAt 0 m
  -- addToVEnv c (TypeScheme [] t1)
  -- t2 <- checkExp (snd (cm Map.! c))


checkExp (Constructor p c) = checkVar p c

checkExp (Fork p e) = do
  t <- checkExp e
  checkUn p t
  return $ TypeScheme [] (Basic UnitType)
  
checkExp (Case p e cm) = do
  t <- checkExp e
--  m <- extractExtChoice p t
  let (c, (x, e1)) = Map.elemAt 0 cm
  t2 <- extractDatatype p t c  
  
  --addToVEnv x (TypeScheme [] t2)
  foldr (\v acc -> addToVEnv v (TypeScheme [] t2)) (return ()) x
  u <- checkExp e1

  Map.foldrWithKey (\k v _ -> checkMap p t u k v extractDatatype) (return ()) (Map.delete c cm)

  return u
  
-- | Case Pos Expression CaseMap

checkMap :: Pos -> TypeScheme -> TypeScheme -> TermVar ->
            (Params, Expression) -> (Pos -> TypeScheme -> Constructor -> TypingState Type) -> TypingState ()
checkMap p choice (TypeScheme _ t) c (x, e) f = do
  t1 <- f{-extractDatatype  ExtChoice-} p choice c
  -- partir o tipo por param
 -- addToVEnv x (TypeScheme [] t1)
  foldr (\v acc -> addToVEnv v (TypeScheme [] t1)) (return ()) x
  checkAgainst p e t
  return ()
  
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

-- Some check funs

checkUn :: Pos -> TypeScheme -> TypingState ()
checkUn p (TypeScheme _ t) = do
  kenv <- getKindEnv
  if un kenv t then    
    return ()
  else
    addError (show p ++ ": Type " ++ show t ++ " is not unrestricted")



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

extractExtChoice :: Pos -> TypeScheme -> Constructor -> TypingState Type
extractExtChoice p (TypeScheme [] (Semi Skip t)) c =  extractExtChoice p (TypeScheme [] t) c
extractExtChoice _ (TypeScheme [] (Choice External m)) c = return $ m Map.! c -- Choice External m
extractExtChoice _ (TypeScheme [] (Semi (Choice External m) t)) c =
  return $ (Map.map (`Semi` t) m) Map.! c -- Choice External (Map.map (`Semi` t) m)
extractExtChoice p (TypeScheme [] (Rec b t)) c = extractExtChoice p (TypeScheme [] (unfold (Rec b t))) c
-- TODO
-- extractExtChoice p (TypeScheme [] (Semi t1 t2)) = do
extractExtChoice p t _ = do
  addError (show p ++  ": Expecting an external choice; found " ++ show t)
  return Skip

extractDatatype :: Pos -> TypeScheme -> Constructor -> TypingState Type
extractDatatype _ (TypeScheme [] (Datatype m)) c = return $ m Map.! c -- Choice External m
extractDatatype p (TypeScheme [] (Rec b t)) c =
  extractDatatype p (TypeScheme [] (unfold (Rec b t))) c
-- TODO
-- extractDatatype p (TypeScheme [] (Semi t1 t2)) = do
extractDatatype p t _ = do
  addError (show p ++  ": Expecting a datatype; found " ++ show t)
  return (Basic IntType)  

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
      -- v2 = Map.insert "myId" t3 v1
      -- v3 = v2 --Map.insert "c" (TypeScheme [] cType) v2
      -- v4 = Map.insert "w" (TypeScheme [] wType) v3
      -- v5 = Map.insert "C" (TypeScheme [] (Fun Un (Basic IntType)(Basic IntType))) v4
      -- v6 = Map.insert "z" (TypeScheme [] (Fun Lin (Basic IntType)(Basic IntType))) v5
      -- v2 = Map.insert "c" (TypeScheme [] cType1) v1
      -- v2 = Map.insert "c1" (TypeScheme [] c1Type) v1
      v2 = Map.insert "l" (TypeScheme [] intlistType) v1
      v3 = Map.insert "IntList" (TypeScheme [] intlistType) v2
  in v2

cType = read "!();!Int;?Bool;Skip" :: Type
wType = read "Skip;Skip;Skip;+{Plus:Int};Skip" :: Type
cType1 = read "&{And: Skip;?Bool;?Bool;!Bool;Skip, Or: Skip;?Int;?Bool;!Bool;Skip, Not: Skip;?Bool;!Bool}" :: Type
c1Type = read "Skip;?Bool;?Int;!Bool;Skip" :: Type

lType = Var "IntList" -- TODO: Param as a variable to datatype
intlistType = read "[Cons: (Int -> (IntList -> IntList)), Nil: IntList]" :: Type

initKindEnv =
  Map.foldrWithKey (\x (TypeScheme _ y) acc ->
                      Map.insert x (kindOf (Map.empty) y) acc) Map.empty initVarEnv
  

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

e11_1 = BinLet (120,13) "n1" "c2" (Receive (120,30) (Variable (120,30) "c1")) (BinLet (121,11) "n2" "c3" (Receive (121,28) (Variable (121,28) "c2")) (UnLet (122,11) "x" (Send (122,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (122,21) "n1")) (Variable (122,27) "n2")) (Variable (122,31) "c3")) (Unit (122,37))))


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
e22 = Select (96,19) "Plusa" (Variable (96,24) "w")

-- Constructor
-- error
e23 = Constructor (100,23) "Tree"
-- send receive
-- add C to init
e24 = Constructor (100,23) "C"

-- Fork
e25 = Fork (1,2) (Integer (3,4) 2)
-- not un
-- add z as Fun Lin Int Int
e26 = Fork (1,2) (Variable (3,4) "z")

-- match - Ok
-- add c as
-- &{And: Skip;?Bool;?Bool;!Bool;Skip, Or: Skip;?Bool;?Bool;!Bool;Skip, Not: Skip;?Bool;!Bool}
-- change type of c to turn into invalid tests
e27 = Match (106,9) (Variable (106,9) "c") (Map.fromList [("And",("c1",BinLet (108,11) "n1" "c2" (Receive (108,28) (Variable (108,28) "c1")) (BinLet (109,11) "n2" "c3" (Receive (109,28) (Variable (109,28) "c2")) (UnLet (110,11) "x" (Send (110,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (110,21) "n1")) (Variable (110,27) "n2")) (Variable (110,31) "c3")) (Unit (111,7)))))),("Or",("c1",BinLet (114,11) "n1" "c2" (Receive (114,28) (Variable (114,28) "c1")) (BinLet (115,11) "n2" "c3" (Receive (115,28) (Variable (115,28) "c2")) (UnLet (116,11) "x" (Send (116,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (116,21) "n1")) (Variable (116,27) "n2")) (Variable (116,31) "c3")) (Unit (117,7))))))])

-- should fail, n1 (not) not in scope
e28 = Match (106,9) (Variable (106,9) "c") (Map.fromList [("And",("c1",BinLet (108,11) "n1" "c2" (Receive (108,28) (Variable (108,28) "c1")) (BinLet (109,11) "n2" "c3" (Receive (109,28) (Variable (109,28) "c2")) (UnLet (110,11) "x" (Send (110,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (110,21) "n1")) (Variable (110,27) "n2")) (Variable (110,31) "c3")) (Unit (111,7)))))),("Not",("c1",BinLet (120,11) "n2" "c3" (Receive (120,28) (Variable (120,28) "c1")) (UnLet (121,11) "x" (Send (121,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (121,21) "n1")) (Variable (121,27) "n2")) (Variable (121,31) "c3")) (Unit (122,7))))),("Or",("c1",BinLet (114,11) "n1" "c2" (Receive (114,28) (Variable (114,28) "c1")) (BinLet (115,11) "n2" "c3" (Receive (115,28) (Variable (115,28) "c2")) (UnLet (116,11) "x" (Send (116,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (116,21) "n1")) (Variable (116,27) "n2")) (Variable (116,31) "c3")) (Unit (117,7))))))])

e29 = Match (106,9) (Variable (106,9) "c") (Map.fromList [("And",("c1",BinLet (108,11) "n1" "c2" (Receive (108,28) (Variable (108,28) "c1")) (BinLet (109,11) "n2" "c3" (Receive (109,28) (Variable (109,28) "c2")) (UnLet (110,11) "x" (Send (110,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (110,21) "n1")) (Variable (110,27) "n2")) (Variable (110,31) "c3")) (Unit (111,7)))))),("Not",("c1",BinLet (120,11) "n2" "c3" (Receive (120,28) (Variable (120,28) "c1")) (UnLet (121,11) "x" (Send (121,20) (App (0,0) (Variable (0,0) "not") (Variable (121,25) "n2")) (Variable (121,29) "c3")) (Unit (122,7))))),("Or",("c1",BinLet (114,11) "n1" "c2" (Receive (114,28) (Variable (114,28) "c1")) (BinLet (115,11) "n2" "c3" (Receive (115,28) (Variable (115,28) "c2")) (UnLet (116,11) "x" (Send (116,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (116,21) "n1")) (Variable (116,27) "n2")) (Variable (116,31) "c3")) (Unit (117,7))))))])
-- 

-- CASE

e30 = Case (107,8) (Variable (107,8) "l") (Map.fromList [("Cons",(["x","y"],Boolean (109,17) False)),("Nil",([],Boolean (108,12) True))])
