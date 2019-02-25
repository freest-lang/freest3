{-|
Module      :  Kinding
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Kinding
( Kind (..)
, kindOf
, kinding
, isWellKinded
, isSessionType
, un
, lin 
, kindOfScheme
, checkAgainstKind
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Utils.Errors
import           Validation.TypingState


-- Determines whether the type is linear or not
lin :: KindEnv -> Type -> Bool
lin _ (Basic _ _)       = False
lin _ (Fun _ m _ _)     = m == Lin
lin _ (PairType _ _ _)  = False
lin _ (Datatype _ _)    = False
lin _ (Skip _)          = False
lin kenv (Semi _ t1 t2) = lin kenv t1 ||  lin kenv t2 -- WAS: && -- WAS : ||
lin _ (Message _ _ _)   = True
lin _ (Choice _ _ _)    = True
lin kenv (Rec p x t)    = lin (Map.insert (var x) (p, (kind x)) kenv) t
lin kenv (Var _ x)        =
  case kenv Map.!? x of
    Just (_,k) -> multiplicity k == Lin
    Nothing    -> -- should not happen
      error $ "Internal error: predicate lin, type var " ++  show x ++ " not in scope"
      

-- Determines whether the type is unrestricted or not
un :: KindEnv -> Type -> Bool
un kenv t = not (lin kenv t)

-- Is a given type contractive?
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi _ t _) = contractive kenv t
contractive kenv (Rec _ _ t)  = contractive kenv t
contractive kenv (Var _ x)    = Map.member x kenv
contractive _    _            = True

-- Used when an error is found
topKind :: Kind
topKind = Kind Functional Lin

-- Check variables
checkVar :: Pos -> TypeVar -> TypingState Kind
checkVar p v = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    file <- getFileName
    addError p [styleRed $ "'" ++ v ++ "'", "is a free variable."]
    addToKenv p v topKind
    return $ topKind

-- Check if a type is a session type
checkSessionType :: Pos -> Type -> Kind ->  TypingState ()
checkSessionType p t k
  | prekind k == Session = return ()
  | otherwise            =
      addError p ["Expecting type", styleRed $ show t,
                  "to be a session type; found kind", styleRed $ show k]

-- Checks if all the elements of a choice are session types
checkChoice :: Pos -> [Kind] -> TypingState Kind
checkChoice p ks
   | all (<= Kind Session Lin) ks = return $ Kind Session Lin
   | otherwise  = do
       addError p ["One of the components in a choice isn't lower than SL"]
       return topKind

-- Check the contractivity of a given type; issue an error if not
checkContractivity :: Pos -> KindEnv -> Type -> TypingState ()
checkContractivity p kenv t
  | contractive kenv t = return ()
  | otherwise          = 
      addError p ["Type", styleRed $ show t, "is not contractive"]

-- Determines the kinding of a type

kinding :: Pos -> TypeScheme -> TypingState Kind
kinding p t = synthetizeScheme p t -- ) initialState

synthetizeScheme :: Pos -> TypeScheme -> TypingState Kind
synthetizeScheme _ (TypeScheme [] t) = synthetize t
synthetizeScheme p (TypeScheme bs t) = do
  foldM_ (\_ b -> addToKenv p (var b) (kind b)) () bs
  synthetize t

synthetize :: Type -> TypingState Kind
synthetize (Skip _)        = return $ Kind Session Un
synthetize (Message _ _ _) = return $ Kind Session Lin
synthetize (Basic _ _)     = return $ Kind Functional Un
synthetize (Var p x)       = checkVar p x
synthetize (Semi p t u) = do -- TODO: Pos
  kt <- synthetize t 
  ku <- synthetize u
  checkSessionType p t kt
  checkSessionType p u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
synthetize (Fun _ m t u) = do
  synthetize t
  synthetize u
  return $ Kind Functional m
synthetize (PairType _ t u) = do
  synthetize t 
  synthetize u
  return $ Kind Functional Lin
synthetize (Datatype _ m) = do
  ks <- mapM synthetize (Map.elems m)
  return $ Kind Functional (multiplicity $ maximum ks)
synthetize (Choice p _ m) = do
  ks <- mapM synthetize (Map.elems m)
  checkChoice p ks  
synthetize (Rec p (Bind x k) t) = do
  addToKenv p x k
  k1 <- synthetize t
  kenv <- getKenv
  checkContractivity p kenv t
  removeFromKenv x
  return k1

{- WAS:
checkAgainstKind :: Pos -> Type -> Kind -> TypingState Type
checkAgainstKind p t k = do
  k' <- synthetize p t
  isSubKindOf k' k
  return t
-}

checkAgainstKind :: Type -> Kind -> TypingState ()
checkAgainstKind t k = do
  k' <- synthetize t
  isSubKindOf (typePos t) k' k
  return ()

isSubKindOf :: Pos -> Kind -> Kind -> TypingState ()
isSubKindOf p k1 k2
  | k1 <= k2  = return ()
  | otherwise = do
      file <- getFileName
      addError p ["Expection kind", styleRed $ show k1, "to be a sub-kind of kind",
                   styleRed $ show k2, "but it isn't."]
      

-- TODO: review & union should not be necessary
-- TODO: filename
kindOf :: KindEnv -> Type -> Kind
kindOf k t =
  let (f, venv, eenv, cenv, kenv, err) = (initialState  "") in
  evalState (synthetize t) (f, venv, eenv, cenv, Map.union k kenv, err)

kindOfScheme :: Pos -> TypeScheme -> Kind
kindOfScheme p t = evalState (synthetizeScheme p t) (initialState "")


isWellKinded :: KindEnv -> Type -> Bool
isWellKinded k t =
  let (f, venv, eenv, cenv, kenv, err) = (initialState "") in
  let (_,_,_,_,_,errors) =
        execState (synthetize t) (f, venv, eenv, cenv, Map.union k kenv, err) in
    null errors

isSessionType :: KindEnv -> Type -> Bool
isSessionType kenv t = isWellKinded kenv t && prekind (kindOf kenv t) == Session
