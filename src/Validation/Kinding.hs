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

module Validation.Kinding
(   Kind (..)
  , kindOf
  , kinding
  , isWellKinded
  , isSessionType
  , contractive
  , un
  , lin 
  , kindErr
  , kindOfScheme
  , checkAgainstKind
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Validation.TypingState

-- Determines whether the type is linear or not
lin :: KindEnv -> Type -> Bool
lin _ (Basic _) = False
lin _ (Fun m _ _) = m == Lin
lin _ (PairType _ _) = False
lin _ (Datatype _) = False
lin _ Skip = False
lin kenv (Semi t1 t2) = lin kenv t1 || lin kenv t2
lin _ (Message _ _) = True
lin _ (Choice _ _) = True
lin kenv (Rec _ t) = lin kenv t
lin kenv (Var x) =
  if Map.member x kenv then multiplicity (kenv Map.! x) == Lin
  else error $ show x

-- Determines whether the type is unrestricted or not
un :: KindEnv -> Type -> Bool
un kenv t = not (lin kenv t)

-- Is a given type contractive?
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi t _) = contractive kenv t
contractive kenv (Rec _ t)  = contractive kenv t
contractive kenv (Var x)    = Map.member x kenv
contractive _    _          = True

-- Used when an error is found
topKind :: Kind
topKind = Kind Functional Lin

-- Check variables
checkVar :: TypeVar -> TypingState Kind
checkVar v = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    addError (show v ++ " is a free variable.")
    return $ topKind

-- Check if a type is a session type
checkSessionType :: Type -> Kind ->  TypingState ()
checkSessionType t k
  | prekind k == Session = return ()
  | otherwise            =
      addError ("Expecting type " ++ show t ++ " to be a session type; found kind " ++ show k)

-- Checks if all the elements of a choice are session types
checkChoice :: [Kind] -> TypingState Kind
checkChoice ks
   | all (<= Kind Session Lin) ks = return $ Kind Session Lin
   | otherwise  = do
       addError ("One of the components in a choice isn't lower than SL")
       return topKind

-- Check the contractivity of a given type; issue an error if not
checkContractivity :: KindEnv -> Type -> TypingState ()
checkContractivity kenv t
  | contractive kenv t = return ()
  | otherwise          = addError ("Type " ++ show t ++ " is not contractive")

-- Determines the kinding of a type

-- TODO ...
-- kinding :: Type -> TypingState Kind
kinding :: Type -> TypingState Kind
kinding t = synthetize t -- ) initialState

synthetize :: Type -> TypingState Kind
synthetize Skip = return $ Kind Session Un
synthetize (Message _ _) = return $ Kind Session Lin
synthetize (Basic _)     = return $ Kind Functional Un
synthetize (Var x)       = checkVar x
synthetize (Semi t u) = do
  kt <- synthetize t 
  ku <- synthetize u
  checkSessionType t kt
  checkSessionType u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
synthetize (Fun m t u) = do
  synthetize t
  synthetize u
  return $ Kind Functional m
synthetize (PairType t u) = do
  synthetize t 
  synthetize u
  return $ Kind Functional Lin
synthetize (Datatype m) = do
  ks <- mapM synthetize (Map.elems m)
  return $ Kind Functional (multiplicity $ maximum ks)
synthetize (Choice _ m) = do
  ks <- mapM synthetize (Map.elems m)
  checkChoice ks  
synthetize (Rec (Bind x k) t) = do
  addToKenv x k
  k1 <- synthetize t
  kenv <- getKindEnv
  checkContractivity kenv t
  return k1


synthetizeScheme :: TypeScheme -> TypingState Kind
synthetizeScheme (TypeScheme [] t) = synthetize t
synthetizeScheme (TypeScheme bs t) = do
  foldM_ (\_ b -> addToKenv (var b) (kind b)) () bs
  synthetize t


-- TODO ...

checkAgainstKind :: Type -> Kind -> TypingState Type
checkAgainstKind t k = do
  k' <- synthetize t
  isSubKindOf k' k
  return t

isSubKindOf :: Kind -> Kind -> TypingState ()
isSubKindOf k1 k2
  | k1 <= k2  = return ()
  | otherwise = addError ("ERROR")
      

-- TODO: review

kindOf :: KindEnv -> Type -> Kind
kindOf k t =
  let (x,y,z) = initialState in
  evalState (synthetize t) (Map.union k x, y, z)

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (synthetizeScheme t) initialState


isWellKinded :: KindEnv -> Type -> Bool
isWellKinded k t =
  let (x,y,z) = initialState in
  let (_, _, err) = execState (synthetize t) (Map.union k x, y, z) in null err

isSessionType :: KindEnv -> Type -> Bool
isSessionType k t = isWellKinded k t && prekind (kindOf k t) == Session

-- Need this ?? 
kindErr :: Type -> [String]
kindErr t =
  let (_, _, err) = execState (synthetize t) initialState in err


-- runState :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

