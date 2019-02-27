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
, checkAgainst
, kinding
, un
, lin 
, kindOfType -- test
, kindOfScheme -- test
, isWellFormed -- test
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Utils.Errors
import           Validation.Contractive
import           Validation.TypingState

-- Returns the kind of a given type scheme
kinding :: Pos -> TypeScheme -> TypingState Kind
kinding _ (TypeScheme [] t) = synthetize t
kinding p (TypeScheme bs t) = do
  foldM_ (\_ b -> addToKenv p (var b) (kind b)) () bs
  synthetize t

-- Returns the kind of a given type
synthetize :: Type -> TypingState Kind
-- Session types
synthetize (Skip _) = return $ Kind Session Un
synthetize (Message _ _ _) = return $ Kind Session Lin
synthetize (Choice p _ m) = do
  ks <- mapM (checkAgainst (Kind Session Lin)) (Map.elems m)
  return (Kind Session Lin)
synthetize (Semi p t u) = do
  kt <- synthetize t 
  ku <- synthetize u
  m <- checkSessionKind p t kt
  n <- checkSessionKind p u ku
  return $ Kind Session (max m n)
-- Functional
synthetize (Basic _ _) = return $ Kind Functional Un
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
synthetize (Rec p (Bind x k) t) = do
{-
  kenv <- getKenv
  checkContractive kenv t
  y <- freshVar                  
  addToKenv y k
  k' <- synthetize (Var p y) x t -- On the fly α-conversion
  -- TODO: use the p in the Bind
  removeFromKenv y
  return k'
-}
  kenv <- getKenv
  checkContractive kenv t
  b <- kenvMember x
  addToKenv p x k
  k1 <- synthetize t
  if b then return ()
  else removeFromKenv x
  return k1
-- Session or functional
synthetize (Var p v) = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    addError p [styleRed $ "'" ++ v ++ "'", "is a free variable."]
    addToKenv p v topKind
    return $ topKind

-- Check whether a given kind is session; issue an error if not. In
-- either case return the multiplicity
checkSessionKind :: Pos -> Type -> Kind -> TypingState Multiplicity
checkSessionKind p t k
  | prekind k == Session = return $ multiplicity k
  | otherwise            = do
      addError p ["Expecting type", styleRed $ show t,
                  "to be a session type; found kind", styleRed $ show k]
      return $ multiplicity k

-- Checks whether a given type has a given kind
checkAgainst :: Kind -> Type -> TypingState ()
checkAgainst k t = do
  k' <- synthetize t
  checkSubkind (typePos t) k' k

-- Checks whether a given kind is a sub kind of another;
-- gives an error message if it isn't
checkSubkind :: Pos -> Kind -> Kind -> TypingState ()
checkSubkind p k1 k2
  | k1 <= k2  = return ()
  | otherwise =
      addError p ["Expecting kind", styleRed $ show k1, "to be a sub-kind of kind",
                   styleRed $ show k2, "but it isn't."]

-- Determines whether a given type is of a given multiplicity
mult :: Multiplicity -> Type -> TypingState Bool
mult m t = do
  k <- synthetize t
  return $ multiplicity k == m
      
-- Determines whether a given type is linear or not
lin :: Type -> TypingState Bool
lin = mult Lin

-- Determines whether a given type is unrestricted or not
un :: Type -> TypingState Bool
un = mult Un

-- Used to insert in the kinding environment when an error is found
topKind :: Kind
topKind = Kind Functional Lin

-- For TESTS only, from here on

kindOfType :: KindEnv -> Type -> Kind
kindOfType k t =
  let (f, venv, eenv, cenv, kenv, err) = (initialState  "") in
  evalState (synthetize t) (f, venv, eenv, cenv, Map.union k kenv, err)

kindOfScheme :: Pos -> TypeScheme -> Kind
kindOfScheme p t = evalState (kinding p t) (initialState "")

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let (f, venv, eenv, cenv, kenv, err) = initialState "" in
  let (_, _, _, _, _, errors) =
        execState (synthetize t) (f, venv, eenv, cenv, k, err) in
    null errors
