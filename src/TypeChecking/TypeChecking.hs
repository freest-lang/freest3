module TypeChecking.TypeChecking (
  typeCheck
) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Kinding
import           Types.Types
import           Terms.Terms
import           Types.TypeEquivalence
import           System.Log.Logger
import           Control.Monad

-- TODO REMOVE
import Types.TypeParser

typeCheck :: Args -> Expression -> TermVar -> VarEnv -> IO(Type)
typeCheck args exp fname venv = do
  debugM "Type Checking" ("Goal: " ++ (show venv) ++ " |- " ++ (show exp))
  -- print venv
  venv1 <- checkExpEnv fname args venv
  -- print venv
  (t, venv2) <- checkExp exp venv1
--  checkVEnvUn venv -- TODO
  debugM "Type Checking" "Done!"
  return t

checkExp :: Expression -> VarEnv -> IO(Type, VarEnv)
-- Basic expressions
checkExp Unit venv = return (Basic UnitType, venv)
checkExp (Integer _) venv = return (Basic IntType, venv)
checkExp (Character _) venv = return (Basic CharType, venv)
checkExp (Boolean _) venv = return (Basic BoolType, venv)
-- Variables
checkExp (Variable x) venv = checkVar x venv
--return (venv Map.! x, venv)
-- Aplication
checkExp (Application e1 e2) venv1 = do
   (t1, venv2) <- checkExp e1 venv1
   (t2, t3) <- checkFun t1
   (t4, venv3) <- checkExp e2 venv2
   checkEquivTypes t2 t4
   return (t3, venv3)
-- Conditional
checkExp (Conditional e1 e2 e3) venv1 = do
  (t1, venv2) <- checkExp e1 venv1
  checkBool t1
  (t2, venv3) <- checkExp e2 venv2
  (t3, venv4) <- checkExp e3 venv3
  checkEquivTypes t2 t3
  checkEquivEnvs venv3 venv4
  return (t2, venv3)
-- Pairs
checkExp (Pair e1 e2) venv1 = do
  (t1, venv2) <- checkExp e1 venv1
  (t2, venv3) <- checkExp e2 venv2
  return (PairType t1 t2, venv3)
checkExp (Let x1 x2 e1 e2) venv1 = do
  (t1, venv2) <- checkExp e1 venv1
  (t2, t3) <- checkPair t1
  (t4, venv3) <- checkExp e2 (Map.insert x2 t3 (Map.insert x1 t2 venv2))
  return (t4, venv3)
-- Session types
checkExp (New t) venv = do
  t <- checkSessionType t
  return (PairType t (dual t), venv)
-- send
checkExp (Send e1 e2) venv1 = do
  (b, venv2) <- checkExp e1 venv1
  (t1, venv3) <- checkExp e2 venv2
  -- TODO t2
  return (Fun Un b (Fun Un t1 t1{-t2-}), venv3)
-- receive
checkExp (Fork e) venv1 = do
  (t, venv2) <- checkExp e venv1
  checkUn t --TODO: review un predicate
  return (Basic UnitType, venv2)
-- Datatypes
-- TODO

{-
checkExp venv1 (Send e1 e2) = (Fun Un b (Fun Un t1 t2), venv3)
  where (b, venv2) = checkExp venv1 e1
        (t1, venv3) = checkExp venv2 e2
  where _ = if isSessionType t2 then () else error "New type is not session type"
-}

checkVar :: TermVar -> VarEnv -> IO (Type,VarEnv)
checkVar x venv
  | Map.member x venv = return (venv Map.! x, venv)
  | otherwise         = do
      errorM "Type Checking" ("Not found " ++ x)
      return (Basic UnitType,venv)

checkEquivTypes :: Type -> Type -> IO()
checkEquivTypes t1 t2
  | equivalent t1 t2 = return ()
  | otherwise        = errorM "Type Checking" ("Expecting type " ++ (show t1) ++ " to be equivalent to type " ++ (show t2))

checkEquivEnvs :: VarEnv -> VarEnv -> IO()
checkEquivEnvs venv1 venv2
  | equivalentEnvs  venv1 venv2 = return ()
  | otherwise                   = errorM "Type Checking"
      ("Expecting enviroment " ++ (show venv1) ++ " to be equivalent to enviroment " ++ (show venv2))

checkFun :: Type -> IO (Type, Type)
checkFun (Fun _ t1 t2) = return (t1, t2)
checkFun t             = do
  errorM "Type Checking" ("Expecting a function type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkPair :: Type -> IO (Type, Type)
checkPair (PairType t1 t2) = return (t1, t2)
checkPair t             = do
  errorM "Type Checking" ("Expecting a pair type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkSessionType :: Type -> IO(Type)
checkSessionType t
  | isSessionType t = return t
  | otherwise       = do
      errorM "Type Checking" ("Expecting a session type; found " ++ (show t))
      return Skip

checkBool :: Type -> IO ()
checkBool (Basic BoolType) = return ()
checkBool t                = errorM "Type Checking" ("Expecting a boolean type; found " ++ (show t))

checkUn :: Type -> IO()
checkUn t
  | un t      = return ()
  | otherwise = errorM "Type Checking" ("Type " ++ show t ++ " is not unrestricted")

{-
-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file
checkExpEnv :: VarEnv -> ExpEnv -> Bool
checkExpEnv venv eenv = Map.foldrWithKey (\fun pair b -> b && checkFun venv fun pair) True eenv

checkFun venv1 fun (args, exp) = checkExp venv2 exp
  where venv2 = venv1 -- TODO: add venv1 fun args
-}

-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file
checkExpEnv :: TermVar -> Args -> VarEnv -> IO (VarEnv)
checkExpEnv fun args venv = foldM (\acc a -> checkParam acc fun a) venv arguments
  where arguments = joinArgsAndType args (venv Map.! fun)
  --checkExpArgs venv fname args
  --foldM (\acc (fun, (args, e)) -> checkExpArgs acc fun args) venv (Map.toList eenv)

-- checkExpArgs :: VarEnv -> TermVar -> Args -> IO (VarEnv)
-- checkExpArgs venv fun args = foldM (\acc a -> checkParam acc fun a) venv arguments
--   where arguments = joinArgsAndType args (venv Map.! fun)
  -- foldr (\a acc -> f a acc) (tt venv) args

checkParam :: VarEnv -> TermVar -> (TermVar,Type) -> IO (VarEnv)
checkParam venv fun (arg,t)
  | Map.member arg venv = do
      errorM "Type Checking" ("Conflicting definitions for '" ++ arg ++ "'\n" ++ "In an equation for '" ++ fun ++ "'")
      return venv
  | otherwise = return $ (Map.insert arg t venv)

-- Same size final and args
joinArgsAndType :: Args -> Type -> [(TermVar,Type)]
joinArgsAndType [] t = [] -- TODO  [("##RET", t)]
joinArgsAndType (x:xs) (Fun _ t1 t2) = [(x,t1)] ++ joinArgsAndType xs t2
joinArgsAndType (x:xs) t = [(x,t)]
  -- error $ "List: " ++ show xs ++ "\nType: " ++ show t

-- Variable environments
equivalentEnvs :: VarEnv -> VarEnv -> Bool
equivalentEnvs venv1 venv2 = Map.size venv1 == Map.size venv2 && equiveElems
  where equiveElems =  Map.foldlWithKey (\b tv t -> b && Map.member tv venv2 &&
                            equivalent t (venv2 Map.! tv)) True venv1

-- Type environments

checkTypeEnv :: TypeEnv -> Bool
checkTypeEnv tenv = Map.foldr (\(_,t) b -> b && isType kindEnv t) True tenv
  where kindEnv = Map.map fst tenv
