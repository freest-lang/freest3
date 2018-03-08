module Types.Kinding
( -- isType
--,
isSessionType
, isSchemeType
, kindOf
, contractive
,Kind (..)
, KindEnv
, un
) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types
import           Data.Either (lefts,rights)
import           Data.List (intercalate)
import           System.Log.Logger
--import           Control.Monad

type KindEnv = Map.Map TypeVar Kind
-- type Message = String
-- type KindingOut = Either Kind Message

loggerName = "Kinding"

-- Kind of a Type

kindOf :: Type -> IO Kind
kindOf t = do
  debugM loggerName ("Goal: KindOf " ++ show t)
  k <- kinding Map.empty t
  debugM loggerName "Done!"
  return k


kinding :: KindEnv -> Type -> IO Kind
kinding _ Skip = return $ Kind Session Un
kinding _ (Out _) = return $ Kind Session Lin
kinding _ (In _) = return $ Kind Session Lin
kinding _ (Basic _) = return $ Kind Arbitrary Un
kinding delta (Var x) = checkVar delta x

kinding delta (Semi t u) = do
  kt <- kinding delta t 
  ku <- kinding delta u
  checkSessionType t kt
  checkSessionType t ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))

kinding delta (Fun m t u) = do
  kt <- kinding delta t 
  ku <- kinding delta u
  checkNotTypeScheme t kt
  checkNotTypeScheme t ku
  return $ Kind Arbitrary m

kinding delta (PairType t u) = do
  kt <- kinding delta t 
  ku <- kinding delta u
  checkNotTypeScheme t kt
  checkNotTypeScheme t ku
  return $ Kind Arbitrary Lin

kinding delta (Datatype m) = do
  ks <- mapM (kinding delta) (Map.elems m)
  checkDataType ks (Kind Arbitrary Un)
      ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m)
  
kinding delta (Choice _ m) = do
  ks <- mapM (kinding delta) (Map.elems m)
  checkTypeMapCases ks (Kind Session Lin)
      ("One of the components in a choice isn't lower than a S^l. " ++ (show m))

kinding delta (Rec x t) = do
  k <- kinding (Map.insert x (Kind Session Un) delta) t
  checkContractivity delta t
  checkNotTypeScheme (Rec x t) k 
  return k

kinding delta (Forall x t) = --do
  return $ Kind Arbitrary Lin
  -- let kd = kinding (Map.insert x (Kind Session Un) delta) t in
  -- case kd of
  --   -- TODO: k is the kinding of the variable and it is always Kind Session Un ?
  --   -- (Left k') | k' >= (Kind Scheme Un) -> Left k'
  --   (Left k') | k' <= (Kind Arbitrary Lin) -> Left k'
  --   (Right m) -> Right m
  --   -- _ -> Right "Forall body is not a type Scheme"

checkTypeMap :: KindEnv -> TypeMap -> Kind -> String -> IO Kind
checkTypeMap delta tm k m = do--liftM $
  ks <- mapM (kinding delta) (Map.elems tm)
  checkTypeMapCases ks k m

checkTypeMapCases :: [Kind] -> Kind -> String -> IO Kind
checkTypeMapCases ks k m
   | all (<= k) ks = return $ Kind Session Lin
   | otherwise  = do
       errorM loggerName m
       return $ Kind Arbitrary Lin

checkDataType :: [Kind] -> Kind -> String -> IO Kind
checkDataType ks k m
   | all (<= k) ks = return $ Kind Arbitrary (multiplicity $ maximum ks)
   | otherwise  = do
       errorM loggerName m
       return $ Kind Arbitrary Lin
  
-- Check if a type is a session type
checkSessionType :: Type -> Kind -> IO ()
checkSessionType t k
  | isSession k = return ()
  | otherwise   = errorM loggerName ("Expecting type " ++ (show t) ++ " to be a session type but it is a " ++ (show k))
      
isSessionType :: Type -> IO Bool
isSessionType t = do
  k <- kindOf t
  return $ isSession k

isSession :: Kind -> Bool
isSession (Kind Session _) = True
isSession _                = False

-- Check if a type is a type scheme

isSchemeType :: Type -> IO Bool
isSchemeType t = do -- isScheme . kindOf
  k <- kindOf t
  return $ isScheme k
  
isScheme :: Kind -> Bool
isScheme (Kind Scheme _) = True
isScheme _               = False

-- Check if a type is not a type scheme
checkNotTypeScheme :: Type -> Kind -> IO ()
checkNotTypeScheme t k
  | not (isScheme k) = return ()
  | otherwise        = errorM loggerName ("Type " ++ (show t) ++ " is a type Scheme")


-- Check variables
checkVar :: KindEnv -> TypeVar -> IO Kind
checkVar delta v 
  | Map.member v delta = return $ delta Map.! v
  | otherwise          = do
      errorM loggerName ("Variable " ++ (show v) ++ " is a free variable")
      return $ Kind Arbitrary Lin

-- Extracts the multiplicity of a kind

multiplicity :: Kind -> Multiplicity
multiplicity (Kind _ m) = m


-- Contractivity
contractive :: KindEnv -> Type -> Bool
contractive delta (Semi t _) = contractive delta t
contractive delta (Rec _ t) = contractive delta t
contractive delta (Var x) = Map.member x delta
contractive delta (Forall _ t) = contractive delta t
contractive _ _ = True

checkContractivity :: KindEnv -> Type -> IO ()
checkContractivity delta t
  | contractive delta t = return ()
  | otherwise           = errorM loggerName ("Type " ++ (show t) ++ " is not contractive.")

-- Check if a type is wellformed 

-- isType :: KindEnv -> Type -> Bool
-- isType kenv t =
--   case kinding kenv t of
--     Left _  -> True
--     Right _ -> False

-- Check if the type's multiplicity is unrestricted

un :: Type -> IO Bool
un t = do --isUn . kindOf
  k <- kindOf t
  return $ isUn k
  
isUn :: Kind -> Bool
isUn (Kind _ Un) = True
isUn (Kind _ _)  = False

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))
