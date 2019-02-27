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
, kindOf -- test
, kinding
, isWellKinded -- test
, isSessionType
, un
, lin 
, kindOfScheme -- test
, checkAgainst
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Utils.Errors
import           Validation.Contractive
import           Validation.TypingState



-- Determines the kinding of a type

kinding :: Pos -> TypeScheme -> TypingState Kind
kinding p t = synthetizeScheme p t -- ) initialState

synthetizeScheme :: Pos -> TypeScheme -> TypingState Kind
synthetizeScheme _ (TypeScheme [] t) = synthetize t
synthetizeScheme p (TypeScheme bs t) = do
  foldM_ (\_ b -> addToKenv p (var b) (kind b)) () bs
  synthetize t

-- The synthetize function:
-- It synthetizes a kind of a given type

synthetize :: Type -> TypingState Kind
synthetize (Skip _)        = return $ Kind Session Un
synthetize (Message _ _ _) = return $ Kind Session Lin
synthetize (Basic _ _)     = return $ Kind Functional Un
synthetize (Var p v)       = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    addError p [styleRed $ "'" ++ v ++ "'", "is a free variable."]
    addToKenv p v topKind
    return $ topKind
synthetize (Semi p t u) = do
  kt <- synthetize t 
  ku <- synthetize u
  checkSessionType p t kt
  checkSessionType p u ku
  -- checkAgainst t (Kind Session Lin)
  -- checkAgainst u (Kind Session Lin)
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
synthetize (Fun _ m t u) = do
  synthetize t
  synthetize u
  return $ Kind Functional m
synthetize (PairType _ t u) = do
  -- kt <- synthetize t
  -- ku <- synthetize u
  -- return $ (max kt ku)
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
  kenv <- getKenv
  checkContractive p kenv t
  b <- kenvMember x
  addToKenv p x k
  k1 <- synthetize t
  if b then return ()
  else removeFromKenv x
  return k1


-- The check against function:
-- Given a type and kind, it checks if the kind
-- of the given type is a sub kind of the given kind

checkAgainst :: Type -> Kind -> TypingState ()
checkAgainst t k = do
  k' <- synthetize t
  checkSubKind (typePos t) k' k
  return ()

-- Checks if one kind is a sub kind of another kind
-- gives an error message if it isn't
checkSubKind :: Pos -> Kind -> Kind -> TypingState ()
checkSubKind p k1 k2
  | k1 <= k2  = return ()
  | otherwise =
      addError p ["Expecting kind", styleRed $ show k1, "to be a sub-kind of kind",
                   styleRed $ show k2, "but it isn't."]

-- Determines whether the type is linear or not
lin :: Type -> TypingState Bool
lin t = do
  k <- synthetize t
  return $ multiplicity k == Lin
      
-- Determines whether the type is unrestricted or not
un :: Type -> TypingState Bool
un t = do
  l <- lin t
  return $ not l

-- Used to insert in the kinding environment when an error is found
topKind :: Kind
topKind = Kind Functional Lin

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


{- WAS: 
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

un :: KindEnv -> Type -> Bool
un kenv t = not (lin kenv t)
-}
