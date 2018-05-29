module TypeChecking.TypeChecking (
    typeCheck
  , TCheckM
  , canonical -- TODO: Remove
  , unfold -- TODO: Remove
) where

import           Control.Monad
import           Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Terms.Terms
import           Types.Kinding
import           Types.Kinds
import           Types.TypeEquivalence
import           Types.Types
import           Types.TypeParser

import Types.TypeParser -- TODO : remove

type TCheckM = Writer [String]


-- Main function
-- It returns unit and possibly error on the writer monad TCheckM

typeCheck :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> TCheckM ()
typeCheck venv eenv cenv kenv = do
  -- 1 - Data declaration
  checkDataDecl kenv cenv
  
  -- 2 - Function type declaration
  pure $ Map.mapWithKey (\fun (a, e) -> checkFunTypeDecl kenv venv fun) eenv

  -- 3 - Function declaration  
  (_, venv1) <- checkVar (0,0) kenv venv "start"

  let venv2 = Map.union venv1 cenv
  let a = Map.mapWithKey (\fun (a, e) -> checkFD venv2 kenv fun a e) eenv
  tell $ Map.foldl (\acc v -> acc ++ execWriter v) [] a

  return ()

-- Checks one function declaration

checkFD :: VarEnv -> KindEnv -> TermVar -> Params -> Expression -> TCheckM ()
checkFD venv kenv fname args exp = do
  venv1 <- checkExpEnv (0,0) kenv venv fname args

  --  checkTypeEnv tenv
  (t, venv2) <- checkExp kenv venv1 exp

  let lastType = last $ toList $ venv Map.! fname

  checkEquivTypes (0,0) kenv t lastType 

  checkVEnvUn kenv venv2
  return ()

-- Checks if an expression is well typed
-- Ensures: the type in the result is canonical
checkExp :: KindEnv -> VarEnv -> Expression -> TCheckM (TypeScheme, VarEnv)

-- Basic expressions
checkExp _ venv (Unit _)         = return (TypeScheme [] (Basic UnitType), venv)
checkExp _ venv (Integer _ _)    = return (TypeScheme [] (Basic IntType), venv)
checkExp _ venv (Character _ _)  = return (TypeScheme [] (Basic CharType), venv)
checkExp _ venv (Boolean _ _)    = return (TypeScheme [] (Basic BoolType), venv)

-- Variables
checkExp kenv venv (Variable p x)  = checkVar p kenv venv x

checkExp kenv venv1 (UnLet _ x e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1
  (t2, venv3) <- checkExp kenv (Map.insert x t1 venv2) e2
  return (t2, venv3)

-- Aplication
checkExp kenv venv1 (App p e1 e2) = do
   (t1, venv2) <- checkExp kenv venv1 e1
   (t2, t3) <- checkFun p t1
   (t4, venv3) <- checkExp kenv venv2 e2
   checkEquivTypes p kenv t2 t4
   return (t3, venv3)

checkExp kenv venv1 (TypeApp p e t) = do
  (t1, venv2) <- checkExp kenv venv1 e
  (v, c) <- checkScheme p kenv t1

  mapM (\t' -> checkKinding kenv (TypeScheme [] t')) t 
  -- checkEquivKinds k k1
  let sub = foldr (\(t', b) acc -> subs t' (var b) acc) c (zip t v) 
  return ((TypeScheme [] sub), venv2)
 
-- Conditional
checkExp kenv venv1 (Conditional p e1 e2 e3) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  checkBool p t1
  (t2, venv3) <- checkExp kenv venv2 e2 
  (t3, venv4) <- checkExp kenv venv3 e3  
  checkEquivTypes p kenv t2 t3
  checkEquivEnvs kenv venv3 venv4
  return (t2, venv3)
 
-- Pairs
checkExp kenv venv1 (Pair _ e1 e2) = do
  (TypeScheme _ t1, venv2) <- checkExp kenv venv1 e1 
  (TypeScheme _ t2, venv3) <- checkExp kenv venv2 e2 
  return (TypeScheme [] (PairType t1 t2), venv3)

checkExp kenv venv1 (BinLet p x1 x2 e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  (t2, t3) <- checkPair p t1
  (t4, venv3) <- checkExp kenv (Map.insert x2 t3 (Map.insert x1 t2 venv2)) e2
  -- TODO: Check
  -- checkVarUn p kenv venv3 x1
  -- checkVarUn p kenv venv3 x2
  let venv4 = Map.delete x1 venv3
      venv5 = Map.delete x2 venv4
  return (t4, venv5) 

-- Session types
checkExp kenv venv (New p t) = do -- TODO
  t <- checkSessionType p kenv t
  let venv1 = checkInternalToVenv venv t
  return (TypeScheme [] (PairType t (dual t)), venv1)

checkExp kenv venv1 (Send p e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  b1 <- checkBasic p t1
  (t2, venv3) <- checkExp kenv venv2 e2
 -- tell ["   |   " ++ show t2 ++ " ### " ++ show (canonical t2) ++ "   |   "]
  (t3, t4) <- checkSemi p (canonical t2)
  b2 <- checkOutType p t3
  checkEquivBasics p b1 b2
  return (canonical (TypeScheme [] t4), venv3)

checkExp kenv venv1 (Receive p e) = do  
  (t1, venv2) <- checkExp kenv venv1 e
  (t2, t3) <- checkSemi p (canonical t1)
  b <- checkInType t2
  let (TypeScheme _ t4) = canonical (TypeScheme [] t3)
  return (TypeScheme [] (PairType (Basic b) t4), venv2)

checkExp kenv venv1 (Select p c e) = do  
  (t, venv2) <- checkExp kenv venv1 e
  let (TypeScheme bs t') = t 
  let venv3 = checkInternalToVenv venv2 t'
  checkInternalChoice p (canonical t)
  (t1, venv4) <- checkVar p kenv venv3 c
  return (canonical t1, venv4)

-- Fork
checkExp kenv venv1 (Fork p e) = do
  (t, venv2) <- checkExp kenv venv1 e 
  checkUn p kenv t
  return (TypeScheme [] (Basic UnitType), venv2)

-- -- Datatypes
checkExp kenv venv (Constructor p c) = checkVar p kenv venv c

checkExp kenv venv1 (Case p e m) = do
  (t, venv2) <- checkExp kenv venv1 e
  -- check datatype?
  checkEquivConstructors p kenv venv2 t m
  l <- checkCaseMap p kenv venv2 e m

  let ts = (map fst l)
  let venvs1 = (map snd l)
  
  checkEquivTypeList p kenv ts
  
  venvs2 <- checkFinalMatchEnvs venvs1 m
 -- checkEquivEnvList kenv venvs2 -- TODO: SendTree fails - details in the end
  return (head ts, head venvs2)

--checkExp kenv venv t = 
checkExp kenv venv1 (Match p e cm) = do
  (t, venv2) <- checkExp kenv venv1 e
  checkExternalChoice p kenv venv2 (canonical t)
  venv3 <- choiceConst venv2 t
  -- checkEquivConstructors kenv venv2 t cm
  l <- checkMatchMap p kenv venv3 e cm

  let ts = (map fst l)
  let venvs1 = (map snd l)
  
  checkEquivTypeList p kenv ts

  venvs2 <- checkFinalEnvs venvs1 cm
  checkEquivEnvList kenv venvs2
  return (head ts, head venvs2)


  

-- Checking variables

checkVar :: Pos -> KindEnv -> VarEnv -> TermVar -> TCheckM (TypeScheme, VarEnv)
checkVar pos kenv venv x
  | Map.member x venv = return $ checkUnLinVar venv (kindOfScheme kenv t) t x
  | otherwise         = do
      tell [show pos ++  ": Variable or data constructor not in scope: " ++ x]
      return (TypeScheme [] (Basic UnitType), venv) 
  where
    t = venv Map.! x
  
checkUnLinVar :: VarEnv -> Kind -> TypeScheme -> TermVar -> (TypeScheme, VarEnv)
checkUnLinVar venv (Kind _ Lin) t var = (t, Map.delete var venv)
checkUnLinVar venv (Kind _ Un) t var  =  (t, venv)

-- Checking equivalent types and environments
checkEquivTypes :: Pos -> KindEnv -> TypeScheme -> TypeScheme -> TCheckM ()
checkEquivTypes pos kenv (TypeScheme _ t1) (TypeScheme _ t2)
  | equivalent kenv t1 t2 = return ()
  | otherwise        = tell [show pos ++  ": Expecting type " ++ (show t1) ++
                              " to be equivalent to type " ++ (show t2)]


-- checkVarUn :: Pos -> KindEnv -> VarEnv -> TermVar -> TCheckM ()
-- checkVarUn p kenv venv v
--   | Map.member v venv = checkUn p kenv (venv Map.! v)
--   | otherwise         = return ()
    

-- Check variable environments

checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TCheckM ()
checkEquivEnvs kenv venv1 venv2 
  | equivalentEnvs kenv venv1 venv2  = return ()
  | otherwise = tell [("Expecting environment " ++ (show venv1) ++
                       " to be equivalent to environment " ++ (show venv2))]
-- New
equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
equivalentEnvs kenv venv1 venv2 =
--  (Map.size venv1 == Map.size venv2) &&
  let venv3 = Map.filter f1 venv1
      venv4 = Map.filter f1 venv2 in
  Map.isSubmapOfBy f venv3 venv4 && Map.isSubmapOfBy f venv4 venv3
  where
    f (TypeScheme b t) (TypeScheme _ u) = {-isUn kenv (TypeScheme b t) || -}equivalent kenv t u
    f1 t = not (isUn kenv t)
  -- (Map.foldlWithKey (equivEnvElem venv2) True venv1)
  -- where 
  --   equivEnvElem :: VarEnv -> Bool -> TermVar -> TypeScheme -> Bool
  --   equivEnvElem venv2 acc tv t =
  --     acc && (checkVarInEnv venv2 tv) && t == (venv2 Map.! tv)
  --      -- && (equivalent kenv t (venv2 Map.! tv))

checkVarInEnv :: VarEnv -> TermVar -> Bool
checkVarInEnv env var = Map.member var env

checkEquivTypeList :: Pos -> KindEnv -> [TypeScheme] -> TCheckM ()
checkEquivTypeList p kenv (x:xs) = mapM_ (checkEquivTypes p kenv x) xs

checkEquivEnvList :: KindEnv -> [VarEnv] -> TCheckM ()
checkEquivEnvList kenv (x:xs) =
  mapM_ (checkEquivEnvs kenv x) xs

checkEquivBasics :: Pos -> BasicType -> BasicType -> TCheckM ()
checkEquivBasics p b1 b2
  | b1 == b2  = return ()
  | otherwise = tell [show p ++ ": Expecting basic type " ++ (show b1) ++
                      " to be equivalent to basic type " ++ (show b2)]

-- Pattern matching against the various type constructors

checkBool :: Pos -> TypeScheme -> TCheckM ()
checkBool _ (TypeScheme _ (Basic BoolType)) = return ()
checkBool p (TypeScheme _ t)                = tell [show p ++ ": Expecting a boolean type; found " ++ (show t)]

checkUnit :: Pos -> TypeScheme -> TCheckM ()
checkUnit _ (TypeScheme _ (Basic UnitType)) = return ()
checkUnit p (TypeScheme _ t) = tell [show p ++ ": Expecting a unit type; found " ++ (show t)]


checkFun :: Pos -> TypeScheme -> TCheckM (TypeScheme, TypeScheme)
checkFun _ (TypeScheme bs (Fun _ t1 t2)) = return (TypeScheme bs t1, TypeScheme bs t2)
checkFun pos (TypeScheme _ t)                           = do
  tell [show pos ++  ": Expecting a function type; found " ++ (show t)]
  return (TypeScheme [] (Basic IntType), TypeScheme [] (Basic IntType))

checkPair :: Pos -> TypeScheme -> TCheckM (TypeScheme, TypeScheme)
checkPair _ (TypeScheme bs (PairType t1 t2)) = return (TypeScheme bs t1, TypeScheme bs t2)
checkPair p (TypeScheme bs t)                = do
  tell [show p ++ ": Expecting a pair type; found " ++ (show t)]
  return (TypeScheme bs (Basic IntType), TypeScheme bs (Basic IntType))

checkScheme :: Pos -> KindEnv -> TypeScheme -> TCheckM ([Bind], Type)
checkScheme p kenv (TypeScheme bs t)
  | not (null bs) = return (bs, t)
  | otherwise     = do
     tell [show p ++ ": Expecting a type scheme; found " ++ show t]
     return ([], (Basic UnitType))
    

checkBasic :: Pos -> TypeScheme -> TCheckM BasicType
checkBasic _ (TypeScheme _ (Basic b)) = return b
checkBasic p (TypeScheme _ t)         = do
  tell [show p ++ ": Expecting a basic type; found " ++ show t]
  return IntType
  
-- -- Checking session types

checkOutType :: Pos -> Type -> TCheckM BasicType
checkOutType _ (Message Out b) = return b
checkOutType p t       = do
  tell [show p ++ ": Expecting an output type; found " ++ (show t)]
  return IntType

checkInType :: Type -> TCheckM BasicType
checkInType (Message In b) = return b
checkInType t      = do
  tell ["Expecting an input type; found " ++ (show t)]
  return IntType

checkSessionType :: Pos -> KindEnv -> Type -> TCheckM Type
checkSessionType p kenv t
  | isSessionType kenv t = return t
  | otherwise = do
          tell [show p ++ ": Expecting a session type; found " ++ (show t)]
          return Skip


checkNotSessionType :: Kind -> TCheckM ()
checkNotSessionType k
  | k >= (Kind Functional Un) = return ()
  | otherwise = tell [("Expecting a functional (TU or TL) type; found a " ++ (show k) ++ " type.")]
          

checkInternalChoice :: Pos -> TypeScheme -> TCheckM ()
checkInternalChoice _ (TypeScheme _ (Choice Internal t)) = return ()
-- new ? 
checkInternalChoice _ (TypeScheme _ (Semi (Choice Internal t) _)) = return () 
checkInternalChoice p (TypeScheme _ t)                   = 
  tell [show p ++ ": Expecting an internal choice; found " ++ show t]

checkExternalChoice :: Pos -> KindEnv -> VarEnv -> TypeScheme -> TCheckM ()
checkExternalChoice _ _ _ (TypeScheme _ (Choice External t)) = return ()
-- new ?
checkExternalChoice _ _ _ (TypeScheme _ (Semi (Choice External t) Skip)) = return ()
checkExternalChoice p kenv venv (TypeScheme _ (Var x))       = do
  (t, venv) <- checkVar p kenv venv x
  checkExternalChoice p kenv venv t 
checkExternalChoice _ _ _ (TypeScheme _ t)                   = 
  tell ["Expecting an external choice; found " ++ show t]

checkSemi :: Pos -> TypeScheme -> TCheckM (Type, Type)
checkSemi _ (TypeScheme bs (Semi t1 t2)) = return (t1, t2)
checkSemi p (TypeScheme _ t)             = do
  tell [show p ++ ": Expecting a sequential session type; found " ++ show t]
  return (Message Out IntType, Skip)

-- Check multiplicity   

checkUn :: Pos -> KindEnv -> TypeScheme -> TCheckM ()
checkUn p kenv t
  | isUn kenv t = return ()
  | otherwise = tell [show p ++ ": Type " ++ show t ++ " is not unrestricted"]

-- Type checking the case constructor

checkMatchMap :: Pos -> KindEnv -> VarEnv -> Expression -> MatchMap -> TCheckM [(TypeScheme, VarEnv)]
checkMatchMap p kenv venv e cm = 
  Map.foldrWithKey' (checkMatchBranch p kenv venv e) (return []) cm

checkMatchBranch :: Pos -> KindEnv -> VarEnv -> Expression -> TypeVar -> (TermVar, Expression) -> TCheckM [(TypeScheme, VarEnv)] -> TCheckM [(TypeScheme, VarEnv)]
checkMatchBranch p kenv venv e c (param, exp) acc = do
  (t, venv1) <- checkVar p kenv venv c
  -- add to list
  paramTypeList <- addToEnv p c [param] (toList t)
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  pair <- checkExp kenv venv2 exp
  pairs <- acc
  return $ pair:pairs

choiceConst :: VarEnv -> TypeScheme -> TCheckM VarEnv
-- TODO: any semi?
choiceConst venv (TypeScheme _ (Semi (Choice m tm) _)) = choiceConst venv (TypeScheme [] (Choice m tm)) 
choiceConst venv (TypeScheme _ (Choice _ tm)) = return $ Map.union (typeToScheme tm) venv
  where typeToScheme = Map.foldrWithKey (\k t acc -> Map.insert k (TypeScheme [] t) acc) Map.empty
choiceConst venv _ = return $ venv

checkFinalEnvs :: [VarEnv] -> MatchMap -> TCheckM [VarEnv]
checkFinalEnvs venvs1 cm = 
  return $ map (\x -> finalEnv x cm) venvs1 
  where
    finalEnv env cm =
      Map.foldlWithKey (\acc k (v,_) -> Map.delete k (Map.delete v acc)) env cm

-- Type checking the match constructor

checkEquivConstructors :: Pos -> KindEnv -> VarEnv -> TypeScheme -> CaseMap -> TCheckM ()
checkEquivConstructors p kenv venv t cm =
   Map.foldrWithKey' (\c _ _ -> checkContructor p kenv venv c t) (return ()) cm

checkContructor :: Pos -> KindEnv -> VarEnv -> TermVar -> TypeScheme -> TCheckM ()
checkContructor p kenv venv c t1 = do
  (t2, _) <- checkVar p kenv venv c
  checkEquivTypes p kenv (last (toList t2)) t1
  return ()

checkCaseMap :: Pos -> KindEnv -> VarEnv -> Expression -> CaseMap -> TCheckM [(TypeScheme, VarEnv)]
checkCaseMap p kenv venv e cm =
   Map.foldrWithKey' (checkCaseBranch p kenv venv e) (return []) cm

checkCaseBranch :: Pos -> KindEnv -> VarEnv -> Expression -> TypeVar -> (Params, Expression) -> TCheckM [(TypeScheme, VarEnv)] -> TCheckM [(TypeScheme, VarEnv)]
checkCaseBranch p kenv venv e c (params, exp) acc = do
  (t, venv1) <- checkVar p kenv venv c
  paramTypeList <- addToEnv p c params (init (toList t))
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  let venv3 = checkExpVar venv2 e (last (toList t))
  pair <- checkExp kenv venv3 exp
  pairs <- acc
  return $ pair:pairs

checkExpVar venv (Variable _ c) t = Map.insert c t venv
checkExpVar venv _ _  = venv

checkFinalMatchEnvs :: [VarEnv] -> CaseMap -> TCheckM [VarEnv]
checkFinalMatchEnvs venvs1 cm = 
  return $ map (\x -> finalEnv x cm) venvs1 
  where
    finalEnv env cm =
      Map.foldlWithKey (\acc k (p,_) -> Map.delete k (delParams acc p)) env cm
    delParams env pl = foldl (\acc k -> Map.delete k acc) env pl 

addToEnv :: Pos -> TypeVar -> Params -> [TypeScheme] -> TCheckM [(TypeVar, TypeScheme)]
addToEnv p c ps ts 
  | length ps == length ts = return $ zip ps (map canonical ts)
  | length ps > length ts = do
      tell ["Function or constructor " ++ show c ++ " is applied to too many arguments"]
      return []
  | length ps < length ts = do
      tell ["Function or constructor " ++ show c ++ " is applied to too few arguments"]
      return []

-- -- Expression environments
-- -- venv contains the entries in the prelude as well as those in the source file

checkInternalToVenv :: VarEnv -> Type -> VarEnv 
checkInternalToVenv venv (Semi (Choice m map) _) = checkInternalToVenv venv (Choice m map)
checkInternalToVenv venv (Choice _ m) = Map.union (typeToScheme m) venv
  where typeToScheme = Map.foldrWithKey (\k t acc -> Map.insert k (TypeScheme [] t) acc) Map.empty
checkInternalToVenv venv _ = venv

checkExpEnv :: Pos -> KindEnv -> VarEnv -> TermVar -> Params -> TCheckM VarEnv
checkExpEnv p kenv venv fun params = do
  checkParam fun params
  (t, venv1) <- checkVar p kenv venv fun
  parameters <- addToEnv p fun params (init (toList t))  
  return $ foldl (\acc (arg, t) -> Map.insert arg t acc) venv1 parameters

checkParam :: TermVar -> Params -> TCheckM ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     tell ["Conflicting definitions for " ++ fun ++
           "'\n" ++ "In an equation for '" ++ fun ++ "'"]
     return ()

checkVEnvUn :: KindEnv -> VarEnv -> TCheckM ()
checkVEnvUn kenv venv = Map.foldlWithKey (\b k t -> checkUn (0,0) kenv t) (return ()) venv

-- Type environments

checkDataDecl :: KindEnv -> ConstructorEnv -> TCheckM ()
checkDataDecl kenv cenv = do
  Map.foldl (\_ k -> checkNotSessionType k) (return ()) kenv
  Map.foldl (\_ t -> checkKinding kenv t) (return ()) cenv 

checkKinding :: KindEnv -> TypeScheme -> TCheckM ()
checkKinding kenv (TypeScheme _ t)
  | isWellKinded kenv t = return ()
  | otherwise = tell (kindErr kenv t)

checkFunTypeDecl :: KindEnv -> VarEnv -> TermVar -> TCheckM ()
checkFunTypeDecl kenv venv  fname = do
  (t, _) <- checkVar (0,0) kenv venv fname
 -- venv3 <- checkChoiceParam t venv2
  checkKinding kenv t
  return ()

-- -- TODO: Change to tell an error
-- -- checkTypeEnv :: TypeEnv -> TCheckM Bool
-- -- checkTypeEnv tenv = return $ Map.foldr (\(_,t) b -> b && isWellKinded kindEnv t) True tenv
-- --   where kindEnv = Map.map fst tenv

{-
Conversion to list head normal form.
TODO: the inductive definition of the output type; a proof that the the function outputs one such type.
-}

canonical :: TypeScheme -> TypeScheme

canonical (TypeScheme b (Semi Skip t)) = canonical (TypeScheme b t)
canonical (TypeScheme b (Semi (Choice cv tm) t2)) =
  canonical $ TypeScheme b (Choice cv (Map.map (canonicalType t2) tm))
canonical (TypeScheme b (Semi t1 t2))  =
  let (TypeScheme _ t1') = canonical (TypeScheme b t1) in
  TypeScheme b (Semi t1' t2)
canonical (TypeScheme b (Rec (Bind x k) t)) =
  canonical $ (TypeScheme b (unfold $ Rec (Bind x k) t))
canonical t            = t


canonicalType :: Type -> Type -> Type
canonicalType t Skip = t
canonicalType t t1 = t1 `Semi` t
-- canonical (TypeScheme b (Semi (Semi t1 t2) t3))       = t

