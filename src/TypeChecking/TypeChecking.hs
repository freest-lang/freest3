module TypeChecking.TypeChecking (
  typeCheck
) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           System.Log.Logger
import           Terms.Terms
import           Types.Kinding
import           Types.Kinds
import           Types.TypeEquivalence
import           Types.Types

-- The name of the logger for type checking
loggerName = "Type Checking"

typeCheck :: Params -> Expression -> TermVar -> VarEnv -> TypeEnv -> IO(Type)
typeCheck args exp fname venv tenv = do
  debugM loggerName ("Goal: " ++ (show venv) ++ " |- " ++ (show exp))

--  let venv1 = checkExpEnv fname args venv
--  venv1 <- checkExpEnv fname args venv
  venv1 <- checkExpEnv fname args (Map.union venv tenv)
  print $ "Fun Name: " ++ fname

  (t, venv2) <- checkExp exp venv1
  let lastType = last $ toList $ venv2 Map.! fname
  checkEquivTypes t lastType  
  
--  checkVEnvUn venv
  debugM loggerName "Done!"
  return t

-- Ensures: the type in the result is canonical
checkExp :: Expression -> VarEnv -> IO(Type, VarEnv)

-- Basic expressions
checkExp Unit venv          = return (Basic UnitType, venv)
checkExp (Integer _) venv   = return (Basic IntType, venv)
checkExp (Character _) venv = return (Basic CharType, venv)
checkExp (Boolean _) venv   = return (Basic BoolType, venv)

-- Variables
checkExp (Variable x) venv = checkVar x venv

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

checkExp (Send e1 e2) venv1 = do
  (t1, venv2) <- checkExp e1 venv1 
  b1 <- checkBasic t1
  (t2, venv3) <- checkExp e2 venv2 
  (t3, t4) <- checkSemi (canonical t2)
  b2 <- checkOutType t3
  checkEquivBasics b1 b2
  return (Fun Un t1 (Fun Un t2 t4), venv3)

checkExp (Receive e) venv1 = do
  (t1, venv2) <- checkExp e venv1
  (t2, t3) <- checkSemi (canonical t1)
  b <- checkInType t2
  return (Fun Un t1 (PairType (Basic b) t3), venv2)

checkExp (Select c e) venv1 = do
  (t,venv2) <- checkExp e venv1 
  return (Choice Internal (Map.singleton c t), venv2) -- TODO: add the other branches to this type

checkExp (Match e m) venv1 = do
  -- TODO
  return (Basic UnitType, venv1)
-- Fork
checkExp (Fork e) venv1 = do
  (t, venv2) <- checkExp e venv1 
  checkUn t
  return (Basic UnitType, venv2)

-- Datatypes
checkExp (Constructor c) venv = checkVar c venv

checkExp (Case e cm) venv1 = do
  (t, venv2) <- checkExp e venv1
  checkEquivConstructors venv2 t cm
  l <- checkCaseMap cm venv2
  checkEquivTypeList (map fst l)
--  checkEquivEnvList (map snd l)
  return $ head l
 
-- Checking variables

checkVar :: TermVar -> VarEnv -> IO (Type, VarEnv)
checkVar x venv
  | Map.member x venv = return (venv Map.! x, venv)
  | otherwise         = do
--      print venv
      errorM loggerName ("Variable or data constructor not in scope: " ++ x)
      return (Basic UnitType, venv)

-- Checking equivalent types and environments

checkEquivTypes :: Type -> Type -> IO ()
checkEquivTypes t1 t2 = do
  b <- equivalent t1 t2
  equiv b
  where
    equiv b
      | b         = return ()
      | otherwise = errorM loggerName
                      ("Expecting type " ++ (show t1) ++
                       " to be equivalent to type " ++ (show t2))

checkEquivEnvs :: VarEnv -> VarEnv -> IO ()
checkEquivEnvs venv1 venv2 = do
  b <- equivalentEnvs venv1 venv2
  equiv b
  where
    equiv b
      | b  = return ()
      | otherwise = errorM loggerName ("Expecting environment " ++ (show venv1) ++
                                       " to be equivalent to environment " ++ (show venv2))

equivalentEnvs :: VarEnv -> VarEnv -> IO Bool
equivalentEnvs venv1 venv2 = do  
  b <- Map.foldlWithKey (equivEnvElem venv2) (return True) venv1
  return $ (Map.size venv1 == Map.size venv2) && b
  where 
    equivEnvElem :: VarEnv -> IO Bool -> TermVar -> Type -> IO Bool
    equivEnvElem venv2 acc tv t = do
      b1 <- acc
      b2 <- checkVarInEnv tv venv2
      b3 <- equivalent t (venv2 Map.! tv)
      return $ b1 && b2 && b3


checkVarInEnv var env = return $ Map.member var env

           -- where equivElems = Map.foldlWithKey (\b tv t -> b && Map.member tv venv2 &&
           --                  equivalent t (venv2 Map.! tv)) True venv1
checkEquivTypeList :: [Type] -> IO ()
checkEquivTypeList (x:xs) = 
  mapM_ (checkEquivTypes x) xs

checkEquivEnvList :: [VarEnv] -> IO ()
checkEquivEnvList (x:xs) =
  mapM_ (checkEquivEnvs x) xs

checkEquivBasics :: BasicType -> BasicType -> IO ()
checkEquivBasics b1 b2
  | b1 == b2  = return ()
  | otherwise = errorM loggerName ("Expecting basic type " ++ (show b1) ++
                                        " to be equivalent to basic type " ++ (show b2))

-- Pattern matching against the various type constructors

checkBool :: Type -> IO ()
checkBool (Basic BoolType) = return ()
checkBool t                = errorM loggerName ("Expecting a boolean type; found " ++ (show t))

checkFun :: Type -> IO (Type, Type)
checkFun (Fun _ t1 t2) = return (t1, t2)
checkFun t             = do
  errorM loggerName ("Expecting a function type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkPair :: Type -> IO (Type, Type)
checkPair (PairType t1 t2) = return (t1, t2)
checkPair t                = do
  errorM loggerName ("Expecting a pair type; found " ++ (show t))
  return (Basic IntType, Basic IntType)

checkSemi :: Type -> IO (Type, Type)
checkSemi (Semi t1 t2) = return (t1, t2)
checkSemi t            = do
  errorM loggerName ("Expecting a sequential session type; found " ++ (show t))
  return (Out IntType, Skip)

checkUn :: Type -> IO ()
checkUn t = do
  b <- un t
  unrestricted b
    where
      unrestricted b
        | b         = return ()
        | otherwise = errorM loggerName ("Type " ++ show t ++ " is not unrestricted")

-- Type checking the case constructor

checkEquivConstructors :: VarEnv -> Type -> CaseMap -> IO ()
checkEquivConstructors venv t cm = do
  Map.foldrWithKey' (\c _ _ -> checkContructor venv c t) (return ()) cm

checkContructor :: VarEnv -> TermVar -> Type -> IO ()
checkContructor venv c t1 = do
  (t2, _) <- checkVar c venv
  checkEquivTypes (last (toList t2)) t1

checkCaseMap :: CaseMap -> VarEnv -> IO [(Type, VarEnv)]
checkCaseMap cm venv =
  Map.foldrWithKey' (checkCaseBranch venv) (return []) cm

checkCaseBranch :: VarEnv -> TypeVar -> (Params, Expression) -> IO [(Type, VarEnv)] -> IO [(Type, VarEnv)]
checkCaseBranch venv c (params, exp) acc = do
  (t, venv1) <- checkVar c venv
  paramTypeList <- addToEnv c params (init (toList t))
  let venv2 = Map.union (Map.fromList paramTypeList) venv1
  pair <- checkExp exp venv2
  pairs <- acc
  return $ pair:pairs

addToEnv :: TypeVar -> Params -> [Type] -> IO [(TypeVar, Type)]
addToEnv c ps ts
  | length ps == length ts = return $ zip ps ts
  | otherwise = do
      errorM loggerName ("Constructor " ++ (show c) ++ "is applied to too few arguments")
      return []

-- Checking session types

checkBasic :: Type -> IO BasicType
checkBasic (Basic b) = return b
checkBasic t         = do
  errorM loggerName ("Expecting a basic type; found " ++ show t)
  return IntType

checkOutType :: Type -> IO BasicType
checkOutType (Out b) = return b
checkOutType t       = do
  errorM loggerName ("Expecting an output type; found " ++ (show t))
  return IntType

checkInType :: Type -> IO BasicType
checkInType (In b) = return b
checkInType t      = do
  errorM loggerName ("Expecting an input type; found " ++ (show t))
  return IntType

checkSessionType :: Type -> IO(Type)
checkSessionType t = do
  b <- isSessionType t
  session b
  where
    session b
      | b         = return t
      | otherwise = do
          errorM loggerName ("Expecting a session type; found " ++ (show t))
          return Skip

-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file

checkExpEnv :: TermVar -> Params -> VarEnv -> IO VarEnv
checkExpEnv fun args venv = do
  checkParam fun args
  (t, venv1) <- checkVar fun venv
  arguments <- addToEnv fun args (init (toList (venv Map.! fun)))
  return $ foldl (\acc (arg, t) -> Map.insert arg t acc) venv1 arguments

checkParam :: TermVar -> Params -> IO ()
checkParam fun args
  | length args == length (Set.fromList args) = return ()
  | otherwise                                = do
     errorM loggerName ("Conflicting definitions for " ++ fun ++
                        "'\n" ++ "In an equation for '" ++ fun ++ "'")
     return ()

checkVEnvUn :: VarEnv -> IO ()
checkVEnvUn venv = do
  l <- sequence $ map un env
  allUn l
  where
    allUn l
      | all (== True) l = return ()
      | otherwise       = do
          errorM loggerName ("Venv must be un") -- TODO: good error message
          return ()
    env = map snd (Map.toList venv)

-- Type environments

-- checkTypeEnv :: TypeEnv -> Bool
-- checkTypeEnv tenv = Map.foldr (\(_,t) b -> b && isType kindEnv t) True tenv
--   where kindEnv = Map.map fst tenv


{-
Conversion to list head normal form.
TODO: the inductive definition of the output type; a proof that the the function outputs one such type.
-}
canonical :: Type -> Type
canonical (Rec x t)     = canonical $ unfold $ Rec x t
canonical (Semi Skip t) = canonical t
-- canonical (Semi t1 t2)  = canonical (Semi (canonical t1) t2)
canonical t             = t
