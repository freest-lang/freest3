module TypeChecking.TypeChecking (
    typeCheck
  , TCheckM
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

type TCheckM = Writer [String]

typeCheck :: KindEnv -> VarEnv -> ConstructorEnv -> Params -> Expression -> TermVar -> TCheckM Type
typeCheck kenv venv cenv args exp fname = do
 
  -- 1 - Data declaration
  checkDataDecl kenv cenv
  
  -- 2 - Function type declaration
  venv2 <- checkFunTypeDecl kenv venv fname

 -- error $ (show venv2) ++ "\n\n\n"

  -- 3 - Function declaration  
  (t, venv3) <- checkVar kenv venv2 "start"
  
  -- Union ??
  venv4 <- checkExpEnv  kenv (Map.union venv3 cenv) fname args
  --  checkTypeEnv tenv

  (t, venv5) <- checkExp kenv venv4 exp

  let lastType = last $ toList $ venv5 Map.! fname

  checkEquivTypes kenv t lastType 

  checkVEnvUn kenv venv5

  return t

-- Ensures: the type in the result is canonical
checkExp :: KindEnv -> VarEnv -> Expression -> TCheckM (Type, VarEnv)

-- Basic expressions
checkExp _ venv  Unit          = return (Basic UnitType, venv)
checkExp _ venv (Integer _)    = return (Basic IntType, venv)
checkExp _ venv (Character _)  = return (Basic CharType, venv)
checkExp _ venv (Boolean _)    = return (Basic BoolType, venv)

-- Variables
checkExp kenv venv (Variable x)  = checkVar kenv venv x

checkExp kenv venv1 (UnLet x e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1
  (t2, venv3) <- checkExp kenv (Map.insert x t1 venv2) e2
  return (t2, venv3)

-- Aplication
checkExp kenv venv1 (App e1 e2) = do
   (t1, venv2) <- checkExp kenv venv1 e1
   (t2, t3) <- checkFun t1
   (t4, venv3) <- checkExp kenv venv2 e2 
   checkEquivTypes kenv t2 t4
   return (t3, venv3)

-- TypeApp Expression Type
checkExp kenv venv1 (TypeApp e t) = do
  (t1, venv2) <- checkExp kenv venv1 e
  (v, c) <- checkForall kenv t1
  checkKinding kenv t
  -- checkEquivKinds k k1
  return (subs t v c, venv2)
  
-- Conditional
checkExp kenv venv1 (Conditional e1 e2 e3) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  checkBool t1
  (t2, venv3) <- checkExp kenv venv2 e2 
  (t3, venv4) <- checkExp kenv venv3 e3  
  checkEquivTypes kenv t2 t3
  checkEquivEnvs kenv venv3 venv4
  return (t2, venv3)

-- Pairs
checkExp kenv venv1 (Pair e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  (t2, venv3) <- checkExp kenv venv2 e2 
  return (PairType t1 t2, venv3)

checkExp kenv venv1 (Let x1 x2 e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  (t2, t3) <- checkPair t1
  (t4, venv3) <- checkExp kenv (Map.insert x2 t3 (Map.insert x1 t2 venv2)) e2
  -- TODO: Check
  checkVarUn kenv venv3 x1
  checkVarUn kenv venv3 x2
  let venv4 = Map.delete x1 venv3
      venv5 = Map.delete x2 venv4
  return (t4, venv5) 

-- Session types
checkExp kenv venv (New t) = do
  t <- checkSessionType kenv t
  return (PairType t (dual t), venv)

checkExp kenv venv1 (Send e1 e2) = do
  (t1, venv2) <- checkExp kenv venv1 e1 
  b1 <- checkBasic t1
  (t2, venv3) <- checkExp kenv venv2 e2
  (t3, t4) <- checkSemi (canonical t2)
  b2 <- checkOutType t3
  checkEquivBasics b1 b2
  return (t4, venv3)

checkExp kenv venv1 (Receive e) = do  
  (t1, venv2) <- checkExp kenv venv1 e
  (t2, t3) <- checkSemi (canonical t1)
  b <- checkInType t2
  return (PairType (Basic b) t3, venv2)
--  return (Fun Un t1 (PairType (Basic b) t3), venv2)

checkExp kenv venv1 (Select c e) = do
  (t,venv2) <- checkExp kenv venv1 e
  -- error $ show (canonical t)
  checkInternalChoice (canonical t)
  checkVar kenv venv1 c

-- Fork
checkExp kenv venv1 (Fork e) = do
  (t, venv2) <- checkExp kenv venv1 e 
  checkUn kenv t
  return (Basic UnitType, venv2)

-- Datatypes
checkExp kenv venv (Constructor c) = checkVar kenv venv c

checkExp kenv venv1 (Match e m) = do
  (t, venv2) <- checkExp kenv venv1 e
  -- check datatype?
  checkEquivConstructors kenv venv2 t m
  l <- checkMatchMap kenv venv2 e m

  let ts = (map fst l)
  let venvs1 = (map snd l)
  
  checkEquivTypeList kenv ts
  
  venvs2 <- checkFinalMatchEnvs venvs1 m
  checkEquivEnvList kenv venvs2
  return (head ts, head venvs2)

--checkExp kenv venv t = 
checkExp kenv venv1 (Case e cm) = do
  (t, venv2) <- checkExp kenv venv1 e
  checkExternalChoice kenv venv2 (canonical t)
  venv3 <- choiceConst venv2 t
  -- checkEquivConstructors kenv venv2 t cm
  l <- checkCaseMap kenv venv3 e cm

  let ts = (map fst l)
  let venvs1 = (map snd l)
  
  checkEquivTypeList kenv ts

  venvs2 <- checkFinalEnvs venvs1 cm
  checkEquivEnvList kenv venvs2
  return (head ts, head venvs2)


-- Checking variables

checkVar :: KindEnv -> VarEnv -> TermVar -> TCheckM (Type, VarEnv)
checkVar kenv venv x
  | Map.member x venv = return $ checkUnLinVar venv (kindOf kenv t) t x -- return (venv Map.! x, venv)
  | otherwise         = do
      tell [("Variable or data constructor not in scope: " ++ x)]
      return (Basic UnitType, venv)
  where
    t = venv Map.! x
  
checkUnLinVar :: VarEnv -> Kind -> Type -> TermVar -> (Type, VarEnv)
checkUnLinVar venv (Kind _ Lin) t var = (t, Map.delete var venv)
checkUnLinVar venv (Kind _ Un) t var  =  (t, venv)

-- Checking equivalent types and environments

checkEquivTypes :: KindEnv -> Type -> Type -> TCheckM ()
checkEquivTypes kenv t1 t2
  | equivalent kenv t1 t2 = return ()
  | otherwise        = tell [("Expecting type " ++ (show t1) ++
                              " to be equivalent to type " ++ (show t2))]

checkVarUn :: KindEnv -> VarEnv -> TermVar -> TCheckM ()
checkVarUn kenv venv v
  | Map.member v venv = checkUn kenv (venv Map.! v)
  | otherwise         = return ()
    

-- Check variable environments

checkEquivEnvs :: KindEnv -> VarEnv -> VarEnv -> TCheckM ()
checkEquivEnvs kenv venv1 venv2 
  | equivalentEnvs kenv venv1 venv2  = return ()
  | otherwise = tell [("Expecting environment " ++ (show venv1) ++
                       " to be equivalent to environment " ++ (show venv2))]

equivalentEnvs :: KindEnv -> VarEnv -> VarEnv -> Bool
equivalentEnvs kenv venv1 venv2 =
  (Map.size venv1 == Map.size venv2) &&
  (Map.foldlWithKey (equivEnvElem venv2) True venv1)
  where 
    equivEnvElem :: VarEnv -> Bool -> TermVar -> Type -> Bool
    equivEnvElem venv2 acc tv t =
      acc && (checkVarInEnv venv2 tv) && (equivalent kenv t (venv2 Map.! tv))

checkVarInEnv :: VarEnv -> TermVar -> Bool
checkVarInEnv env var = Map.member var env

checkEquivTypeList :: KindEnv -> [Type] -> TCheckM ()
checkEquivTypeList kenv (x:xs) = mapM_ (checkEquivTypes kenv x) xs

checkEquivEnvList :: KindEnv -> [VarEnv] -> TCheckM ()
checkEquivEnvList kenv (x:xs) =
  mapM_ (checkEquivEnvs kenv x) xs

checkEquivBasics :: BasicType -> BasicType -> TCheckM ()
checkEquivBasics b1 b2
  | b1 == b2  = return ()
  | otherwise = tell ["Expecting basic type " ++ (show b1) ++
                      " to be equivalent to basic type " ++ (show b2)]

-- Pattern matching against the various type constructors

checkBool :: Type -> TCheckM ()
checkBool (Basic BoolType) = return ()
checkBool t                = tell [("Expecting a boolean type; found " ++ (show t))]

checkFun :: Type -> TCheckM (Type, Type)
checkFun (Fun _ t1 t2) = return (t1, t2)
checkFun (Forall _ _  t) = checkFun t
checkFun t             = do
  tell [("Expecting a function type; found " ++ (show t))]
  return (Basic IntType, Basic IntType)

checkPair :: Type -> TCheckM (Type, Type)
checkPair (PairType t1 t2) = return (t1, t2)
checkPair t                = do
  tell [("Expecting a pair type; found " ++ (show t))]
  return (Basic IntType, Basic IntType)

-- TODO: Maybe should return the kenv
checkForall :: KindEnv -> Type -> TCheckM (TypeVar, Type)
checkForall kenv (Forall x _ t) = return (x, t)
checkForall kenv t = do
  tell ["Expecting a forall type; found " ++ show t]
  return ("", (Basic UnitType))

checkBasic :: Type -> TCheckM BasicType
checkBasic (Basic b) = return b
checkBasic t         = do
  tell [("Expecting a basic type; found " ++ show t)]
  return IntType
  
-- Checking session types

checkOutType :: Type -> TCheckM BasicType
checkOutType (Out b) = return b
checkOutType t       = do
  tell ["Expecting an output type; found " ++ (show t)]
  return IntType

checkInType :: Type -> TCheckM BasicType
checkInType (In b) = return b
checkInType t      = do
  tell ["Expecting an input type; found " ++ (show t)]
  return IntType

checkSessionType :: KindEnv -> Type -> TCheckM Type
checkSessionType kenv t
  | isSessionType kenv t = return t
  | otherwise = do
          tell [("Expecting a session type; found " ++ (show t))]
          return Skip

checkNotSessionType :: Kind -> TCheckM ()
checkNotSessionType k
  | k >= (Kind Functional Un) = return ()
  | otherwise = tell [("Expecting a functional (TU or TL) type; found a " ++ (show k) ++ " type.")]
          

checkInternalChoice :: Type -> TCheckM ()
checkInternalChoice (Choice Internal t) = return ()
--checkInternalChoice (Semi (Choice Internal t1) t2) = return ()
checkInternalChoice t                   = 
  tell ["Expecting an internal choice; found " ++ show t]

checkExternalChoice :: KindEnv -> VarEnv -> Type -> TCheckM ()
checkExternalChoice _ _ (Choice External t) = return ()
checkExternalChoice kenv venv (Var x)       = do
  (t, venv) <- checkVar kenv venv x
  checkExternalChoice kenv venv t 
checkExternalChoice _ _ t                    = 
  tell ["Expecting an external choice; found " ++ show t]

checkSemi :: Type -> TCheckM (Type, Type)
checkSemi (Semi t1 t2) = return (t1, t2)
checkSemi t            = do
  tell ["Expecting a sequential session type; found " ++ (show t)]
  return (Out IntType, Skip)

-- Check multiplicity   

checkUn :: KindEnv -> Type -> TCheckM ()
checkUn kenv t
  | un kenv t = return ()
  | otherwise = tell ["Type " ++ show t ++ " is not unrestricted"]

-- Type checking the case constructor

checkCaseMap :: KindEnv -> VarEnv -> Expression -> CaseMap -> TCheckM [(Type, VarEnv)]
checkCaseMap kenv venv e cm = 
  Map.foldrWithKey' (checkCaseBranch kenv venv e) (return []) cm

checkCaseBranch :: KindEnv -> VarEnv -> Expression -> TypeVar -> (TermVar, Expression) -> TCheckM [(Type, VarEnv)] -> TCheckM [(Type, VarEnv)]
checkCaseBranch kenv venv e c (param, exp) acc = do
  (t, venv1) <- checkVar kenv venv c
  -- add to list
  paramTypeList <- addToEnv c [param] ( (toList t))
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  pair <- checkExp kenv venv2 exp
  pairs <- acc
  return $ pair:pairs

choiceConst :: VarEnv -> Type -> TCheckM VarEnv
choiceConst venv (Choice _ tm) = return $ Map.union tm venv
choiceConst venv _ = return $ venv

checkFinalEnvs :: [VarEnv] -> CaseMap -> TCheckM [VarEnv]
checkFinalEnvs venvs1 cm = 
  return $ map (\x -> finalEnv x cm) venvs1 
  where
    finalEnv env cm =
      Map.foldlWithKey (\acc k (v,_) -> Map.delete k (Map.delete v acc)) env cm

-- Type checking the match constructor

checkEquivConstructors :: KindEnv -> VarEnv -> Type -> MatchMap -> TCheckM ()
checkEquivConstructors kenv venv t cm =
   Map.foldrWithKey' (\c _ _ -> checkContructor kenv venv c t) (return ()) cm

checkContructor :: KindEnv -> VarEnv -> TermVar -> Type -> TCheckM ()
checkContructor kenv venv c t1 = do
  (t2, _) <- checkVar kenv venv c
  checkEquivTypes kenv (last (toList t2)) t1
  return ()

checkMatchMap :: KindEnv -> VarEnv -> Expression -> MatchMap -> TCheckM [(Type, VarEnv)]
checkMatchMap kenv venv e cm =
   Map.foldrWithKey' (checkMatchBranch kenv venv e) (return []) cm

checkMatchBranch :: KindEnv -> VarEnv -> Expression -> TypeVar -> (Params, Expression) -> TCheckM [(Type, VarEnv)] -> TCheckM [(Type, VarEnv)]
checkMatchBranch kenv venv e c (params, exp) acc = do
  (t, venv1) <- checkVar kenv venv c
  paramTypeList <- addToEnv c params (init (toList t))
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  let venv3 = checkExpVar venv2 e (last (toList t))
  pair <- checkExp kenv venv3 exp
  pairs <- acc
  return $ pair:pairs

checkExpVar venv (Variable c) t = Map.insert c t venv
checkExpVar venv _ _  = venv

checkFinalMatchEnvs :: [VarEnv] -> MatchMap -> TCheckM [VarEnv]
checkFinalMatchEnvs venvs1 cm = 
  return $ map (\x -> finalEnv x cm) venvs1 
  where
    finalEnv env cm =
      Map.foldlWithKey (\acc k (p,_) -> Map.delete k (delParams acc p)) env cm
    delParams env pl = foldl (\acc k -> Map.delete k acc) env pl 

addToEnv :: TypeVar -> Params -> [Type] -> TCheckM [(TypeVar, Type)]
addToEnv c ps ts 
  | length ps == length ts = return $ zip ps (map canonical ts)
  | length ps > length ts = do
      tell ["Function or constructor " ++ (show c) ++ " is applied to too many arguments"]
      return []
  | length ps < length ts = do
      tell ["Function or constructor " ++ (show c) ++ " is applied to too few arguments"]
      return []

-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file

-- TODO: Can call checkVar? it will delete un functions?
checkExpEnv :: KindEnv -> VarEnv -> TermVar -> Params -> TCheckM VarEnv
checkExpEnv kenv venv fun params = do
  checkParam fun params
  (t, venv1) <- checkVar kenv venv fun
  parameters <- addToEnv fun params (init (toList t))  
--  parameters <- addToEnv fun params (init (toList (venv Map.! fun)))
  return $ foldl (\acc (arg, t) -> Map.insert arg t acc) venv1 parameters



checkParam :: TermVar -> Params -> TCheckM ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     tell ["Conflicting definitions for " ++ fun ++
           "'\n" ++ "In an equation for '" ++ fun ++ "'"]
     return ()

checkVEnvUn :: KindEnv -> VarEnv -> TCheckM ()
checkVEnvUn kenv venv = Map.foldlWithKey (\b k t -> checkUn kenv t) (return ()) venv

-- Type environments

checkDataDecl :: KindEnv -> ConstructorEnv -> TCheckM ()
checkDataDecl kenv cenv = do
  Map.foldl (\_ k -> checkNotSessionType k) (return ()) kenv
  Map.foldl (\_ t -> checkKinding kenv t) (return ()) cenv 

checkKinding :: KindEnv -> Type -> TCheckM ()
checkKinding kenv t -- = tell [ show $ isType kenv t, show t, show kenv]
  | isType kenv t = return ()
  | otherwise = tell (kindErr kenv t)

checkFunTypeDecl :: KindEnv -> VarEnv -> TermVar -> TCheckM VarEnv
checkFunTypeDecl kenv venv  fname = do
  (t, venv2) <- checkVar kenv venv fname
 -- venv3 <- checkChoiceParam t venv2
  checkKinding kenv t
  return venv2



-- TODO: Change to tell an error
-- checkTypeEnv :: TypeEnv -> TCheckM Bool
-- checkTypeEnv tenv = return $ Map.foldr (\(_,t) b -> b && isType kindEnv t) True tenv
--   where kindEnv = Map.map fst tenv

{-
Conversion to list head normal form.
TODO: the inductive definition of the output type; a proof that the the function outputs one such type.
-}
canonical :: Type -> Type
canonical (Rec x k t)     = canonical $ unfold $ Rec x k t
canonical (Semi Skip t) = canonical t
canonical (Semi (Choice cv tm) t2) =
  Choice cv (Map.map (\t -> if t == Skip then t2 else t `Semi` t2) tm)
canonical (Semi t1 t2)  = Semi (canonical t1) t2
canonical t             = t
