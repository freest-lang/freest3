module Types.Kinding
( isType
, isSessionType
--, isSchemeType
, kindOf
, kindErr
, contractive
, Kind (..)
, KindEnv
, un
) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types
import           Data.Either (lefts,rights)
import           Data.List (intercalate)
-- import           System.Log.Logger
--import           Control.Monad
import Control.Monad.Writer

type KindM = Writer [String]

type KindEnv = Map.Map TypeVar Kind
-- type Message = String
-- type KindingOut = Either Kind Message

-- Kind of a Type

kindOf :: KindEnv -> Type -> Kind
kindOf kenv t = kindOf' (kinding kenv t)
  where
    kindOf' :: KindM Kind -> Kind
    kindOf' = fst . runWriter

kinding :: KindEnv -> Type -> KindM Kind
kinding _ Skip = return $ Kind Session Un
kinding _ (Out _) = return $ Kind Session Lin
kinding _ (In _) = return $ Kind Session Lin
kinding _ (Basic _) = return $ Kind Functional Un
kinding kenv (Var x) = checkVar kenv x

kinding kenv (Semi t u) = do
  kt <- kinding kenv t 
  ku <- kinding kenv u
  checkSessionType t kt
  checkSessionType u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))

kinding kenv (Fun m t u) = do
  kt <- kinding kenv t
  ku <- kinding kenv u
  checkNotTypeScheme t kt
  checkNotTypeScheme u ku
  return $ Kind Functional m

kinding kenv (PairType t u) = do
  kt <- kinding kenv t 
  ku <- kinding kenv u
  checkNotTypeScheme t kt
  checkNotTypeScheme t ku
  return $ Kind Functional Lin

kinding kenv (Datatype m) = do
  ks <- mapM (kinding kenv) (Map.elems m)
  checkDataType ks (Kind Functional Un)
      ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m)
  
kinding kenv (Choice _ m) = do
  ks <- mapM (kinding kenv) (Map.elems m)
  checkTypeMapCases ks (Kind Session Lin)
      ("One of the components in a choice isn't lower than a S^l. " ++ (show m))

kinding kenv (Rec (Bind x k) t) = do
  -- TODO: Maybe the kinding function should also return kenv
  let kenv1 = (Map.insert x k kenv)
  k <- kinding kenv1 t
  checkContractivity kenv1 t
  checkNotTypeScheme (Rec (Bind x k) t) k 
  return k

kinding kenv (Forall x k t) = do
  k1 <- kinding (Map.insert x k kenv) t
  -- TODO: Check k1 >= C^u
  return $ k1
  -- let kd = kinding (Map.insert x (Kind Session Un) kenv) t in
  -- case kd of
  --   -- TODO: k is the kinding of the variable and it is always Kind Session Un ?
  --   -- (Left k') | k' >= (Kind Scheme Un) -> Left k'
  --   (Left k') | k' <= (Kind Functional Lin) -> Left k'
  --   (Right m) -> Right m
  --   -- _ -> Right "Forall body is not a type Scheme"

checkTypeMap :: KindEnv -> TypeMap -> Kind -> String -> KindM Kind
checkTypeMap kenv tm k m = do--liftM $
  ks <- mapM (kinding kenv) (Map.elems tm)
  checkTypeMapCases ks k m

checkTypeMapCases :: [Kind] -> Kind -> String -> KindM Kind
checkTypeMapCases ks k m
   | all (<= k) ks = return $ Kind Session Lin
   | otherwise  = do
       tell [m]
       return $ Kind Functional Lin

checkDataType :: [Kind] -> Kind -> String -> KindM Kind
checkDataType ks k m
   | all (<= k) ks = return $ Kind Functional (multiplicity $ maximum ks)
   | otherwise  = do
       tell [m]
       return $ Kind Functional Lin
  
-- Check if a type is a session type
checkSessionType :: Type -> Kind ->  KindM ()
checkSessionType t k
  | isSession k = return ()
  | otherwise   = tell ["Expecting type " ++ (show t) ++ " to be a session type but it is a " ++ (show k)]
      
isSessionType :: KindEnv -> Type -> Bool
isSessionType kenv t =  isSession (kindOf kenv t)
-- isSession . fst $ runWriter (kindOf t)
-- do
--   k <- kindOf t
--   return $ isSession k

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                = False

-- Check if a type is a type scheme

-- isSchemeType :: Type -> Bool
-- isSchemeType =  isScheme . kindOf
--   -- do
--   -- k <- kindOf t
--   -- return $ isScheme k
  
-- isScheme :: Kind -> Bool
-- isScheme (Kind Scheme _) = True
-- isScheme _               = False

-- Check if a type is not a type scheme
checkNotTypeScheme :: Type -> Kind ->  KindM ()
checkNotTypeScheme t k
  | k <= Kind Functional Lin = return ()
  | otherwise        = tell ["Type " ++ (show t) ++ " is a type Scheme"]


-- Check variables
checkVar :: KindEnv -> TypeVar -> KindM Kind
checkVar kenv v 
  | Map.member v kenv = return $ kenv Map.! v
  | otherwise          = do
      tell ["Variable " ++ (show v) ++ " is a free variable"]
      return $ Kind Functional Lin

-- Extracts the multiplicity of a kind

multiplicity :: Kind -> Multiplicity
multiplicity (Kind _ m) = m


-- Contractivity
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi t _) = contractive kenv t
contractive kenv (Rec _ t) = contractive kenv t
contractive kenv (Var x) = Map.member x kenv
contractive kenv (Forall _ _ t) = contractive kenv t
contractive _ _ = True

checkContractivity :: KindEnv -> Type -> KindM ()
checkContractivity kenv t
  | contractive kenv t = return ()
  | otherwise           = tell ["Type " ++ (show t) ++ " is not contractive."]

isType :: KindEnv -> Type -> Bool
isType kenv t = wellFormed (kinding kenv t)
  where
    wellFormed :: KindM Kind -> Bool
    wellFormed = null . snd . runWriter

kindErr :: KindEnv -> Type -> [String]
kindErr kenv t = err (kinding kenv t)
  where
    err :: KindM Kind -> [String]
    err = snd . runWriter
-- fst . runWriter
-- Check if a type is wellformed 

-- isType :: KindEnv -> Type -> Bool
-- isType kenv t =
--   case kinding kenv t of
--     Left _  -> True
--     Right _ -> False

-- Check if the type's multiplicity is unrestricted

un :: KindEnv -> Type -> Bool
un kenv t = isUn (kindOf kenv t)
  -- isUn . fst $ runWriter (kindOf t)
  -- do --isUn . kindOf
  -- k <- kindOf t
  -- return $ isUn k
  
isUn :: Kind -> Bool
isUn (Kind _ Un) = True
isUn (Kind _ _)  = False

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))
