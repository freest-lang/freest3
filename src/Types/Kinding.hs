{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Types.Kinding
( Kind (..)
, KindEnv
<<<<<<< HEAD
, isType
, isSessionType
--, isSchemeType
=======
>>>>>>> 0ee90bda6154fd550c8487d6357acad9f69a255a
, kindOf
, isWellKindedType
, isSessionType
, isContractive
, isUn
, kindErr
<<<<<<< HEAD
, contractive
, kindOfScheme
, un
=======
>>>>>>> 0ee90bda6154fd550c8487d6357acad9f69a255a
) where

import qualified Data.Map.Strict as Map
import Types.Kinds
import Types.Types
import Control.Monad.Writer

type KindM = Writer [String]

type KindEnv = Map.Map TypeVar Kind

kindOf :: KindEnv -> Type -> Kind
kindOf kenv t = fst $ runWriter (kinding kenv t)

kinding :: KindEnv -> Type -> KindM Kind
kinding _    Skip       = return $ Kind Session Un
kinding _    (Out _)    = return $ Kind Session Lin
kinding _    (In _)     = return $ Kind Session Lin
kinding _    (Basic _)  = return $ Kind Functional Un
kinding kenv (Var x)    = checkVar kenv x
kinding kenv (Semi t u) = do
  kt <- kinding kenv t 
  ku <- kinding kenv u
  checkSessionType t kt
  checkSessionType u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
kinding kenv (Fun m t u) = do
  kinding kenv t
  kinding kenv u
--  checkNotTypeScheme t kt
--  checkNotTypeScheme u ku
  return $ Kind Functional m
kinding kenv (PairType t u) = do
  kinding kenv t 
  kinding kenv u
  -- checkNotTypeScheme t kt
  -- checkNotTypeScheme t ku
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
  -- checkNotTypeScheme (Rec (Bind x k) t) k 
  return k
<<<<<<< HEAD

-- TODO: ADD A Kinding function to typeschemes
-- kinding kenv (Forall x k t) = do
--   k1 <- kinding (Map.insert x k kenv) t
--   return $ k1
  -- let kd = kinding (Map.insert x (Kind Session Un) kenv) t in
  -- case kd of
  --   -- TODO: k is the kinding of the variable and it is always Kind Session Un ?
  --   -- (Left k') | k' >= (Kind Scheme Un) -> Left k'
  --   (Left k') | k' <= (Kind Functional Lin) -> Left k'
  --   (Right m) -> Right m
  --   -- _ -> Right "Forall body is not a type Scheme"
-- fst . runWriter
kindOfScheme :: KindEnv -> TypeScheme -> Kind
kindOfScheme kenv t = kinds (kindOfScheme' kenv t)
  where kinds = fst . runWriter 

kindOfScheme' :: KindEnv -> TypeScheme -> KindM Kind
kindOfScheme' kenv (TypeScheme bs t) = do
  k1 <- kinding (toMap kenv bs) t
  return k1
  where toMap kenv = foldr (\b acc -> Map.insert (var b) (kind b) acc) kenv  

=======
kinding kenv (Forall x k t) = do
  k1 <- kinding (Map.insert x k kenv) t
  -- TODO: Check k1 >= C^u
  return $ k1

-- Used when an error is found
topKind :: Kind
topKind = Kind Functional Lin
>>>>>>> 0ee90bda6154fd550c8487d6357acad9f69a255a

checkTypeMap :: KindEnv -> TypeMap -> Kind -> String -> KindM Kind
checkTypeMap kenv tm k m = do--liftM $
  ks <- mapM (kinding kenv) (Map.elems tm)
  checkTypeMapCases ks k m

checkTypeMapCases :: [Kind] -> Kind -> String -> KindM Kind
checkTypeMapCases ks k m
   | all (<= k) ks = return $ Kind Session Lin
   | otherwise  = do
       tell [m]
       return $ topKind

checkDataType :: [Kind] -> Kind -> String -> KindM Kind
checkDataType ks k m
   | all (<= k) ks = return $ Kind Functional (multiplicity $ maximum ks)
   | otherwise  = do
       tell [m]
       return $ topKind
  
-- Check if a type is a session type
checkSessionType :: Type -> Kind ->  KindM ()
checkSessionType t k
  | prekind k == Session = return ()
  | otherwise            = tell ["Expecting type " ++ show t ++ " to be a session type; found kind " ++ show k]

-- Check variables
checkVar :: KindEnv -> TypeVar -> KindM Kind
checkVar kenv v 
  | Map.member v kenv = return $ kenv Map.! v
  | otherwise         = do
      tell ["Variable " ++ show v ++ " is free"]
      return $ topKind

-- Is a given type contractive?
isContractive :: KindEnv -> Type -> Bool
isContractive kenv (Semi t _) = isContractive kenv t
isContractive kenv (Rec _ t)  = isContractive kenv t
isContractive kenv (Var x)    = Map.member x kenv
isContractive kenv (Forall _ _ t) = isContractive kenv t
isContractive _    _          = True

-- Check the contractivity of a given type; issue an error if not

checkContractivity :: KindEnv -> Type -> KindM ()
checkContractivity kenv t
  | isContractive kenv t = return ()
  | otherwise            = tell ["Type " ++ show t ++ " is not contractive"]

isWellKindedType :: KindEnv -> Type -> Bool
isWellKindedType kenv t = null $ snd $ runWriter (kinding kenv t)

isSessionType :: KindEnv -> Type -> Bool
isSessionType kenv t =  prekind (kindOf kenv t) == Session

isUn :: KindEnv -> Type -> Bool
isUn kenv t = multiplicity (kindOf kenv t) == Un 

kindErr :: KindEnv -> Type -> [String]
kindErr kenv t = err (kinding kenv t)
  where
    err :: KindM Kind -> [String]
    err = snd . runWriter

-- These should yield Left _
-- kinding Map.empty (UnFun Skip Skip)
-- kinding Map.empty (UnFun (Basic IntType) (Basic IntType))
-- kinding Map.empty (UnFun (In IntType) (In IntType))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
-- (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType),("c",Basic CharType)]))
