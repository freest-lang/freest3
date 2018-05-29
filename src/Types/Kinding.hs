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
, kindOf
, isWellKinded
, isSessionType
, isContractive
, isUn
, kindErr
, kindOfScheme
) where

import qualified Data.Map.Strict as Map
import Types.Kinds
import Types.Types
import Control.Monad.Writer

type KindM = Writer [String]

type KindEnv = Map.Map TypeVar Kind

kinding :: KindEnv -> Type -> KindM Kind
kinding _    Skip          = return $ Kind Session Un
kinding _    (Message _ _) = return $ Kind Session Lin
kinding _    (Basic _)     = return $ Kind Functional Un
kinding kenv (Var x)       = checkVar kenv x
kinding kenv (Semi t u) = do
  kt <- kinding kenv t 
  ku <- kinding kenv u
  checkSessionType t kt
  checkSessionType u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
kinding kenv (Fun m t u) = do
  kinding kenv t
  kinding kenv u
  return $ Kind Functional m
kinding kenv (PairType t u) = do
  kinding kenv t 
  kinding kenv u
  return $ Kind Functional Lin
kinding kenv (Datatype m) = do
  ks <- mapM (kinding kenv) (Map.elems m)
  checkDatatype ks (Kind Functional Un)
      ("One of the components in a Datatype is a type Scheme. \nType: " ++ show m) -- TODO: ??? type scheme ?
kinding kenv (Choice _ m) = do
  ks <- mapM (kinding kenv) (Map.elems m)
  checkChoice ks
kinding kenv (Rec (Bind x k) t) = do
  -- TODO: Maybe the kinding function should also return kenv
  let kenv1 = (Map.insert x k kenv)
  k <- kinding kenv1 t
  checkContractivity kenv1 t
  return k
  
-- Used when an error is found
topKind :: Kind
topKind = Kind Functional Lin

checkChoice :: [Kind] -> KindM Kind
checkChoice ks
   | all (<= Kind Session Lin) ks = return $ Kind Session Lin
   | otherwise  = do
       tell ["One of the components in a choice isn't lower than SL"]
       return topKind

checkDatatype :: [Kind] -> Kind -> String -> KindM Kind
checkDatatype ks k m
   | all (<= k) ks = return $ Kind Functional (multiplicity $ maximum ks)
   | otherwise  = do
       tell [m]
       return topKind
  
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

-- Check the contractivity of a given type; issue an error if not
checkContractivity :: KindEnv -> Type -> KindM ()
checkContractivity kenv t
  | isContractive kenv t = return ()
  | otherwise            = tell ["Type " ++ show t ++ " is not contractive"]

-- Is a given type contractive?
isContractive :: KindEnv -> Type -> Bool
isContractive kenv (Semi t _) = isContractive kenv t
isContractive kenv (Rec _ t)  = isContractive kenv t
isContractive kenv (Var x)    = Map.member x kenv
isContractive _    _          = True

-- Predicates and functions based on kinding

kindOf :: KindEnv -> Type -> Kind
kindOf kenv t = fst $ runWriter (kinding kenv t)

isWellKinded :: KindEnv -> Type -> Bool
isWellKinded kenv t = null $ snd $ runWriter (kinding kenv t)

isSessionType :: KindEnv -> Type -> Bool
isSessionType kenv t = isWellKinded kenv t && prekind (kindOf kenv t) == Session

kindErr :: KindEnv -> Type -> [String]
kindErr kenv t = err (kinding kenv t)
  where
    err :: KindM Kind -> [String]
    err = snd . runWriter

-- Type Schemes

isUn :: KindEnv -> TypeScheme -> Bool
isUn kenv t = multiplicity (kindOfScheme kenv t) == Un 

kindOfScheme :: KindEnv -> TypeScheme -> Kind
kindOfScheme kenv (TypeScheme [] t) = kindOf kenv t
kindOfScheme kenv t = kinds (kindOfScheme' kenv t)
  where kinds = fst . runWriter 

kindOfScheme' :: KindEnv -> TypeScheme -> KindM Kind
kindOfScheme' kenv (TypeScheme bs t) = do
  k1 <- kinding (toMap kenv bs) t
  return k1
  where toMap kenv = foldr (\b acc -> Map.insert (var b) (kind b) acc) kenv  
