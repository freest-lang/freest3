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

typeCheck :: Args -> Expression -> TermVar -> VarEnv -> TypeEnv -> IO(Type)
typeCheck args exp fname venv tenv = do
  debugM loggerName ("Goal: " ++ (show venv) ++ " |- " ++ (show exp))

--  let venv1 = checkExpEnv fname args venv
--  venv1 <- checkExpEnv fname args venv
  venv1 <- checkExpEnv fname args (Map.union venv tenv)
  print $ "Fun Name: " ++ fname

  (t,_) <- checkExp exp venv1
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

-- Fork
checkExp (Fork e) venv1 = do
  (t, venv2) <- checkExp e venv1 
  checkUn t --TODO: review un predicate
  return (Basic UnitType, venv2)

-- Datatypes
checkExp (Select c e) venv1 = do
  (t,venv2) <- checkExp e venv1 
  return (Choice Internal (Map.singleton c t), venv2)

checkExp (Value c) venv = checkVar c venv
--  t <- checkConstructor c 
--  return (venv Map.! c, venv)

checkExp (Case e m) venv1 = do
  (t, venv2) <- checkExp e venv1
  checkEquivConstructors t m venv2
-- l = [(Type,Env)]
  l <- checkCaseMap m venv2
  checkEquivTypeList (map fst l)
  checkEquivEnvList (map snd l)
  return $ head l


checkVar :: TermVar -> VarEnv -> IO (Type, VarEnv)
checkVar x venv
  | Map.member x venv = return (venv Map.! x, venv)
  | otherwise         = do
      errorM loggerName ("Variable not in scope: " ++ x)
      return (Basic UnitType, venv)

checkEquivTypes :: Type -> Type -> IO ()
checkEquivTypes t1 t2
  | equivalent t1 t2 = return ()
  | otherwise        = errorM loggerName
      ("Expecting type " ++ (show t1) ++ " to be equivalent to type " ++ (show t2))

checkEquivEnvs :: VarEnv -> VarEnv -> IO ()
checkEquivEnvs venv1 venv2
  | equivalentEnvs  venv1 venv2 = return ()
  | otherwise                   = errorM loggerName
      ("Expecting environment " ++ (show venv1) ++ " to be equivalent to environment " ++ (show venv2))

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


-- TODO: same number of args as in the datatype
-- test haskell 
-- type CaseMap = (Map.Map Constructor ([TermVar], Expression))
checkCaseMap :: CaseMap -> VarEnv -> IO ([(Type,VarEnv)])
checkCaseMap cm venv = do
  let venv1 = Map.foldlWithKey (\acc c (tv, _) -> insert c tv acc) venv cm 
  l <- mapM (\e -> checkExp e venv1) exps
  return l
  where
--    vars = map fst (Map.elems cm)
    exps = map snd (Map.elems cm)
    types c tv venv1 = joinArgsAndType tv (venv1 Map.! c)
    insert c tv venv1 = foldl (\acc (k, v) -> Map.insert k v acc) venv1 (types c tv venv1)

-- joinArgsAndType :: Args -> Type -> [(TermVar, Type)]
-- checkExpEnv :: TermVar -> Args -> VarEnv -> IO VarEnv

checkEquivTypeList :: [Type] -> IO ()
checkEquivTypeList (x:xs) = 
  mapM_ (checkEquivTypes x) xs

checkEquivEnvList :: [VarEnv] -> IO ()
checkEquivEnvList (x:xs) =
  mapM_ (checkEquivEnvs x) xs

checkEquivConstructors :: Type -> CaseMap -> VarEnv -> IO ()
checkEquivConstructors t m venv = do
  b <- allEquiv
  checkConstruct b t m
  where
    allEquiv = do
      l <- (mapM (\k -> equivConstructType t k venv) (Map.keys m))
      return $ all (== True) l

checkConstruct :: Bool -> Type -> CaseMap -> IO ()  
checkConstruct b t m
  | b         = return ()
  | otherwise =
      errorM loggerName ("Expecting type " ++ (show t) ++
                              " to be equivalent to all case types " ++ (show m))

equivConstructType :: Type -> Constructor -> VarEnv -> IO Bool
equivConstructType t1 k venv
  | Map.member k venv = do
      let t2 = lastType (venv Map.! k)
      return $ equivalent t1 t2
  | otherwise         = do
      errorM loggerName ("Not in scope: data constructor '" ++ k ++ "'")
      return False

lastType :: Type -> Type
lastType (Fun _ t1 t2) = lastType t2
lastType t             = t

checkBasic :: Type -> IO BasicType
checkBasic (Basic b) = return b
checkBasic t         = do
  errorM loggerName ("Expecting a basic type; found " ++ show t)
  return IntType

checkEquivBasics :: BasicType -> BasicType -> IO ()
checkEquivBasics b1 b2
  | b1 == b2  = return ()
  | otherwise = errorM loggerName ("Expecting basic type " ++ (show b1) ++
                                        " to be equivalent to basic type " ++ (show b2))

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
checkSessionType t
  | isSessionType t = return t
  | otherwise       = do
      errorM loggerName ("Expecting a session type; found " ++ (show t))
      return Skip

checkUn :: Type -> IO ()
checkUn t
  | un t      = return ()
  | otherwise = errorM loggerName ("Type " ++ show t ++ " is not unrestricted")


-- Expression environments
-- venv contains the entries in the prelude as well as those in the source file

checkExpEnv :: TermVar -> Args -> VarEnv -> IO VarEnv
checkExpEnv fun args venv = do
  checkParam fun args
  return $ foldl (\acc (arg, t) -> Map.insert arg t acc) venv arguments
-- checkExpEnv fun args venv = foldl (\acc (arg, t) -> Map.insert arg t acc) venv arguments
  where arguments = joinArgsAndType args (venv Map.! fun)

checkParam :: TermVar -> Args -> IO ()
checkParam fun args =  unique Set.empty args
  where
    unique _ [] = return ()
    unique s (a:as)
      | a `Set.member` s = do         
         errorM loggerName ("Conflicting definitions for " ++ a ++
                                 "'\n" ++ "In an equation for '" ++ fun ++ "'")
         unique s as
      | otherwise        = unique (Set.insert a s) as

--   where allUnique xs = nub xs == xs
--   where allUnique xs = length xs == length (Set.fromList xs)

-- Same size final and args
joinArgsAndType :: Args -> Type -> [(TermVar, Type)]
joinArgsAndType [] t                 = [] -- TODO  [("##RET", t)]
joinArgsAndType (x:xs) (Fun _ t1 t2) = [(x,t1)] ++ joinArgsAndType xs t2
joinArgsAndType (x:xs) t             = [(x,t)]

-- Variable environments
equivalentEnvs :: VarEnv -> VarEnv -> Bool
equivalentEnvs venv1 venv2 = Map.size venv1 == Map.size venv2 && equiveElems
  where equiveElems =  Map.foldlWithKey (\b tv t -> b && Map.member tv venv2 &&
                            equivalent t (venv2 Map.! tv)) True venv1

checkVEnvUn :: VarEnv -> IO ()
checkVEnvUn venv
  | all (== True) $ map un env = return ()
  | otherwise                  = do
      errorM loggerName ("Venv must be un") -- TODO: good error message
      return ()
  where env = map snd (Map.toList venv)

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
canonical (Semi t1 t2)  = canonical (Semi (canonical t1) t2)
canonical t             = t
