module TypeChecking.TypeChecking (
    typeCheck
  , TCheckM
) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Terms.Terms
import           Types.Kinding
import           Types.Kinds
import           Types.TypeEquivalence
import           Types.Types

-- test only
import PreludeLoader

import Control.Monad.Writer

type TCheckM = Writer [String]

typeCheck :: VarEnv -> TypeEnv -> Params -> Expression -> TermVar -> TCheckM Type
typeCheck venv tenv args exp fname = do
    
  -- Union ?? 
  venv1 <- checkExpEnv (Map.union venv tenv) fname args


  (t, venv2) <- checkExp venv1 exp
  let lastType = last $ toList $ venv2 Map.! fname
  checkEquivTypes t lastType  
    
--  checkVEnvUn venv
  return t

-- Ensures: the type in the result is canonical
checkExp :: VarEnv -> Expression -> TCheckM (Type, VarEnv)

-- Basic expressions
checkExp venv  Unit          = return (Basic UnitType, venv)
checkExp venv (Integer _)    = return (Basic IntType, venv)
checkExp venv (Character _)  = return (Basic CharType, venv)
checkExp venv (Boolean _)    = return (Basic BoolType, venv)

-- Variables
checkExp venv (Variable x) = checkVar venv x

-- Aplication
checkExp venv1 (Application e1 e2) = do
   (t1, venv2) <- checkExp venv1 e1 
   (t2, t3) <- checkFun t1
   (t4, venv3) <- checkExp venv2 e2 
   checkEquivTypes t2 t4
   return (t3, venv3)

-- Conditional
checkExp venv1 (Conditional e1 e2 e3) = do
  (t1, venv2) <- checkExp venv1 e1 
  checkBool t1
  (t2, venv3) <- checkExp venv2 e2 
  (t3, venv4) <- checkExp venv3 e3  
  checkEquivTypes t2 t3
  checkEquivEnvs venv3 venv4
  return (t2, venv3)

-- Pairs
checkExp venv1 (Pair e1 e2) = do
  (t1, venv2) <- checkExp venv1 e1 
  (t2, venv3) <- checkExp venv2 e2 
  return (PairType t1 t2, venv3)

checkExp venv1 (Let x1 x2 e1 e2) = do
  (t1, venv2) <- checkExp venv1 e1 
  (t2, t3) <- checkPair t1
  (t4, venv3) <- checkExp (Map.insert x2 t3 (Map.insert x1 t2 venv2)) e2
  return (t4, venv3) -- TODO omit this return

-- Session types
checkExp venv (New t) = do
  t <- checkSessionType t
  return (PairType t (dual t), venv)

checkExp venv1 (Send e1 e2) = do
  (t1, venv2) <- checkExp venv1 e1 
  b1 <- checkBasic t1
  (t2, venv3) <- checkExp venv2 e2 
  (t3, t4) <- checkSemi (canonical t2)
  b2 <- checkOutType t3
  checkEquivBasics b1 b2
  return (Fun Un t1 (Fun Un t2 t4), venv3)

checkExp venv1 (Receive e) = do
  (t1, venv2) <- checkExp venv1 e
  (t2, t3) <- checkSemi (canonical t1)
  b <- checkInType t2
  return (Fun Un t1 (PairType (Basic b) t3), venv2)

checkExp venv1 (Select c e) = do
  (t,venv2) <- checkExp venv1 e 
  return (Choice Internal (Map.singleton c t), venv2) -- TODO: add the other branches to this type

-- checkExp venv1 (Match e m) = do
--   -- TODO
--   return (Basic UnitType, venv1)
-- Fork
checkExp venv1 (Fork e) = do
  (t, venv2) <- checkExp venv1 e 
  checkUn t
  return (Basic UnitType, venv2)

-- Datatypes
checkExp venv (Constructor c) = checkVar venv c

checkExp venv1 (Case e cm) = do
  (t, venv2) <- checkExp venv1 e
  checkEquivConstructors venv2 t cm
  l <- checkCaseMap venv2 cm
  checkEquivTypeList (map fst l)
  checkEquivEnvList (map snd l)
  return $ head l
 
-- Checking variables

checkVar :: VarEnv -> TermVar -> TCheckM (Type, VarEnv)
checkVar venv x
  | Map.member x venv = return (venv Map.! x, venv)
  | otherwise         = do
--      print venv
      tell [("Variable or data constructor not in scope: " ++ x)]
      return (Basic UnitType, venv)

-- -- Checking equivalent types and environments

checkEquivTypes :: Type -> Type -> TCheckM ()
checkEquivTypes t1 t2
  | equivalent t1 t2 = return ()
  | otherwise        = tell [("Expecting type " ++ (show t1) ++
                              " to be equivalent to type " ++ (show t2))]

checkEquivEnvs :: VarEnv -> VarEnv -> TCheckM ()
checkEquivEnvs venv1 venv2 
  | equivalentEnvs venv1 venv2  = return ()
  | otherwise = tell [("Expecting environment " ++ (show venv1) ++
                       " to be equivalent to environment " ++ (show venv2))]

equivalentEnvs :: VarEnv -> VarEnv -> Bool
equivalentEnvs venv1 venv2 =
  (Map.size venv1 == Map.size venv2) &&
  (Map.foldlWithKey (equivEnvElem venv2) True venv1)
  where 
    equivEnvElem :: VarEnv -> Bool -> TermVar -> Type -> Bool
    equivEnvElem venv2 acc tv t =
      acc && (checkVarInEnv venv2 tv) && (equivalent t (venv2 Map.! tv))

checkVarInEnv :: VarEnv -> TermVar -> Bool
checkVarInEnv env var = Map.member var env

--            -- where equivElems = Map.foldlWithKey (\b tv t -> b && Map.member tv venv2 &&
--            --                  equivalent t (venv2 Map.! tv)) True venv1
checkEquivTypeList :: [Type] -> TCheckM ()
checkEquivTypeList (x:xs) = 
  mapM_ (checkEquivTypes x) xs

checkEquivEnvList :: [VarEnv] -> TCheckM ()
checkEquivEnvList (x:xs) =
  mapM_ (checkEquivEnvs x) xs

checkEquivBasics :: BasicType -> BasicType -> TCheckM ()
checkEquivBasics b1 b2
  | b1 == b2  = return ()
  | otherwise = tell ["Expecting basic type " ++ (show b1) ++
                                        " to be equivalent to basic type " ++ (show b2)]

-- -- Pattern matching against the various type constructors

checkBool :: Type -> TCheckM ()
checkBool (Basic BoolType) = return ()
checkBool t                = tell [("Expecting a boolean type; found " ++ (show t))]

checkFun :: Type -> TCheckM (Type, Type)
checkFun (Fun _ t1 t2) = return (t1, t2)
checkFun t             = do
  tell [("Expecting a function type; found " ++ (show t))]
  return (Basic IntType, Basic IntType)

checkPair :: Type -> TCheckM (Type, Type)
checkPair (PairType t1 t2) = return (t1, t2)
checkPair t                = do
  tell [("Expecting a pair type; found " ++ (show t))]
  return (Basic IntType, Basic IntType)

checkSemi :: Type -> TCheckM (Type, Type)
checkSemi (Semi t1 t2) = return (t1, t2)
checkSemi t            = do
  tell ["Expecting a sequential session type; found " ++ (show t)]
  return (Out IntType, Skip)

checkUn :: Type -> TCheckM ()
checkUn t
  | un t      = return ()
  | otherwise = tell ["Type " ++ show t ++ " is not unrestricted"]

-- Type checking the case constructor

checkEquivConstructors :: VarEnv -> Type -> CaseMap -> TCheckM ()
checkEquivConstructors venv t cm = do
  Map.foldrWithKey' (\c _ _ -> checkContructor venv c t) (return ()) cm

checkContructor :: VarEnv -> TermVar -> Type -> TCheckM ()
checkContructor venv c t1 = do
  (t2, _) <- checkVar venv c
  checkEquivTypes (last (toList t2)) t1

checkCaseMap :: VarEnv -> CaseMap -> TCheckM [(Type, VarEnv)]
checkCaseMap venv cm =
  Map.foldrWithKey' (checkCaseBranch venv) (return []) cm

checkCaseBranch :: VarEnv -> TypeVar -> (Params, Expression) -> TCheckM [(Type, VarEnv)] -> TCheckM [(Type, VarEnv)]
checkCaseBranch venv c (params, exp) acc = do
  (t, venv1) <- checkVar venv c
  paramTypeList <- addToEnv c params (init (toList t))
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  pair <- checkExp venv2 exp
  pairs <- acc
  return $ pair:pairs

addToEnv :: TypeVar -> Params -> [Type] -> TCheckM [(TypeVar, Type)]
addToEnv c ps ts
  | length ps == length ts = return $ zip ps ts
  | otherwise = do
      tell ["Constructor " ++ (show c) ++ "is applied to too few arguments"]
      return []

-- Checking session types

checkBasic :: Type -> TCheckM BasicType
checkBasic (Basic b) = return b
checkBasic t         = do
  tell [("Expecting a basic type; found " ++ show t)]
  return IntType

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

checkSessionType :: Type -> TCheckM Type
checkSessionType t
  | isSessionType t = return t
  | otherwise = do
          tell [("Expecting a session type; found " ++ (show t))]
          return Skip

-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file

checkExpEnv :: VarEnv -> TermVar -> Params -> TCheckM VarEnv
checkExpEnv venv fun params = do
  checkParam fun params
  (t, venv1) <- checkVar venv fun
  parameters <- addToEnv fun params (init (toList (venv Map.! fun)))
  return $ foldl (\acc (arg, t) -> Map.insert arg t acc) venv1 parameters

checkParam :: TermVar -> Params -> TCheckM ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     tell ["Conflicting definitions for " ++ fun ++
                        "'\n" ++ "In an equation for '" ++ fun ++ "'"]
     return ()

-- checkVEnvUn :: VarEnv -> IO ()
-- checkVEnvUn venv = do
--   l <- sequence $ map un env
--   allUn l
--   where
--     allUn l
--       | all (== True) l = return ()
--       | otherwise       = do
--           tell ["Venv must be un")] -- TODO: good error message
--           return ()
--     env = map snd (Map.toList venv)

-- Type environments

-- -- checkTypeEnv :: TypeEnv -> Bool
-- -- checkTypeEnv tenv = Map.foldr (\(_,t) b -> b && isType kindEnv t) True tenv
-- --   where kindEnv = Map.map fst tenv


{-
Conversion to list head normal form.
TODO: the inductive definition of the output type; a proof that the the function outputs one such type.
-}
canonical :: Type -> Type
canonical (Rec x t)     = canonical $ unfold $ Rec x t
canonical (Semi Skip t) = canonical t
-- canonical (Semi t1 t2)  = canonical (Semi (canonical t1) t2)
canonical t             = t
