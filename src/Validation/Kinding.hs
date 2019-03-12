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

import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Utils.Errors
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Validation.Contractive
import           Validation.TypingState

-- Returns the kind of a given type scheme
kinding :: TypeScheme -> TypingState Kind
kinding (TypeScheme bs t) = do
  -- TODO: addToKenv -> addBindsLToKenv
  foldM_ (\_ b -> addToKenv (0,0) (var b) (kind b)) () bs
  synthetize t

-- Returns the kind of a given type
synthetize :: Type -> TypingState Kind
-- Session types
synthetize (Skip _) =
  return $ Kind Session Un
synthetize (Message _ _ _) =
  return $ Kind Session Lin
synthetize (Choice _ _ m) = do
  mapM_ (checkAgainst (Kind Session Lin)) (Map.elems m)
  return $ Kind Session Lin
synthetize (Semi _ t u) = do
  kt <- synthetize t 
  ku <- synthetize u
  m <- checkSessionKind t kt
  n <- checkSessionKind u ku
  return $ Kind Session (max m n)
-- Functional
synthetize (Basic _ _) =
  return $ Kind Functional Un
synthetize (Fun _ m t u) = do
  synthetize t
  synthetize u
  return $ Kind Functional m
synthetize (PairType _ t u) = do
  kt <- synthetize t
  ku <- synthetize u
  return $ max kt ku
synthetize (Datatype _ m) = do
  ks <- mapM synthetize (Map.elems m)
  return $ Kind Functional $ multiplicity $ maximum ks
synthetize (Rec _ (p,x) t) = do
  checkContractive t
  y <- freshVar
  addToKenv p y (Kind Session Un)
  k <- synthetize $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv y
  return k
-- Session or functional
synthetize (Var p v) = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    addError p ["Variable not in scope: ", styleRed v]
    addToKenv p v topKind
    return topKind

-- Check whether a given kind is session; issue an error if not. In
-- either case return the multiplicity
checkSessionKind :: Type -> Kind -> TypingState Multiplicity
checkSessionKind t k
  | prekind k == Session = return $ multiplicity k
  | otherwise            = do
      addError (position t) ["Expecting type", styleRed $ show t,
                  "to be a session type; found kind", styleRed $ show k]
      return $ multiplicity k

-- Check whether a given type has a given kind
checkAgainst :: Kind -> Type -> TypingState ()
-- checkAgainst k (Rec _ (Bind x p _) t) = do
checkAgainst k (Rec _ (p, x) t) = do
  checkContractive t
  y <- freshVar
  addToKenv p y (Kind Session Un)
  checkAgainst k$ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv y
checkAgainst k t = do
  k' <- synthetize t
  checkSubkind (position t) k' k

-- Checks whether a given kind is a sub kind of another;
-- gives an error message if it isn't
checkSubkind :: Pos -> Kind -> Kind -> TypingState ()
checkSubkind p k1 k2
  | k1 <= k2  = return ()
  | otherwise =
      addError p ["Expecting kind", styleRed $ show k1, "to be a sub-kind of kind of kind", styleRed $ show k2]

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
  let (f, venv, eenv, cenv, _, err, n) = (initialState  "") in
  evalState (synthetize t) (f, venv, eenv, cenv, k, err, n)

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (kinding t) (initialState "")

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let (f, venv, eenv, cenv, _, err, n) = initialState "" in
  let (_, _, _, _, _, errors, _) =
        execState (synthetize t) (f, venv, eenv, cenv, k, err, n) in
    null errors
