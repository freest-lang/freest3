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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Kinding
( checkAgainst
, synthetise
, synthetiseTS
, isSessionType
, un
--, lin
, fromTBindKs
) where

import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Bind
import           Validation.Contractive
import           Parse.Lexer (Pos, position)
import           Utils.FreestState
import           Utils.Errors
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Debug.Trace

-- Returns the kind of a given type
synthetise :: KindEnv -> Type -> FreestState Kind
  -- Session types
synthetise _ (Skip p) =
  return $ Kind p Session Un
synthetise _ (Message p _ _) =
  return $ Kind p Session Lin
synthetise kenv (Choice p _ m) = do
  tMapM (checkAgainst kenv (Kind p Session Lin)) m
  return $ Kind p Session Lin
synthetise kenv (Semi p t u) = do
  m <- checkAgainstSession kenv t
  n <- checkAgainstSession kenv u
  return $ Kind p Session (max m n)
-- Functional
synthetise _ (Basic p _) =
  return $ Kind p Functional Un
synthetise kenv v@(Fun p m t u) = do
--  trace ("5_K.synthetise " ++ show  v ++ "\nKind Env: " ++ show kenv) (return ())
  synthetise kenv t
  synthetise kenv u
--  trace ("6_K.synthetise " ++ show  v ++ "\nKind Env: " ++ show kenv) (return ())
  return $ Kind p Functional m
synthetise kenv (PairType _ t u) = do
  kt <- synthetise kenv t
  ku <- synthetise kenv u
  return $ lub kt ku
synthetise kenv (Datatype p m) = do
  ks <- tMapM (synthetise kenv) m
  let Kind _ _ n = foldr1 lub ks
  return $ Kind p Functional n
synthetise kenv (Rec p b@(TBindK _ _ k) t) = do
  checkContractive kenv t
  y <- freshVar
  k' <- synthetise (Map.insert (TBind p y) k kenv) $ subs (TypeVar p y) b t -- On the fly α-conversion
  return k'
-- Session or functional
synthetise kenv (TypeVar p x) = do
--  trace ("4_K.synthetise " ++ show x ++ "\nKind Env: " ++ show kenv ) (return ())
  case kenv Map.!? (TBind p x) of
    Just k ->
      return k
    Nothing -> do
      addError p ["Type variable not in scope:", styleRed x]
      return (omission p)
-- Type operators
synthetise kenv (Name p c) =
  let bind = TBind p c in
  getFromTenv bind >>= \case
    Just (k, _) ->
      return k
    Nothing -> do
      addError p ["Type name not in scope:", styleRed c]
      let k = omission p
      addToTenv bind k $ omission p
      return k
synthetise kenv (Dualof p t) = do
  m <- checkAgainstSession kenv t
  return $ Kind p Session m

-- Check whether a given type is of a session kind. In any case return
-- the multiplicity of the kind of the type
checkAgainstSession :: KindEnv -> Type -> FreestState Multiplicity
checkAgainstSession kenv t = do
  (Kind _ k m) <- synthetise kenv t
  when (k /= Session) $
    addError (position t) ["Expecting a session type; found", styleRed $ show t]
  return m

-- Check a type against a given kind
checkAgainst :: KindEnv -> Kind -> Type -> FreestState ()
checkAgainst kenv k (Rec p x t) = do
  checkContractive kenv t
  y <- freshVar
  checkAgainst (Map.insert (TBind p y) (Kind p Session Un) kenv) k $ subs (TypeVar p y) x t -- On the fly α-conversion
checkAgainst kenv expected t = do
  actual <- synthetise kenv t
  when (not (actual <: expected)) $
    addError (position t) ["Couldn't match expected kind", styleRed $ show expected, "\n",
                            "\t with actual kind", styleRed $ show actual, "\n",
                            "\t for type", styleRed $ show t]

synthetiseTS :: TypeScheme -> FreestState Kind
synthetiseTS (TypeScheme _ bs t) = synthetise (fromTBindKs bs) t

fromTBindKs :: [TBindK] -> KindEnv
fromTBindKs = foldr (\(TBindK p x k) env -> Map.insert (TBind p x) k env) Map.empty
--Map.fromList $ map (\(TBindK p x k) -> ((TBind p x), k)) bsol

-- Determine whether a given type is linear
-- lin :: TypeScheme -> FreestState Bool
-- lin = mult Lin

-- Determine whether a given type is unrestricted
un :: TypeScheme -> FreestState Bool
un = mult Un

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> TypeScheme -> FreestState Bool
mult m1 ts = do
  (Kind _ _ m2) <- synthetiseTS ts
  return $ m2 == m1

-- Assumes the type is well formed
isSessionType :: TypeEnv -> KindEnv -> Type -> Bool
  -- Session types
isSessionType _ _ (Skip _)          = True
isSessionType _ _ (Semi _ _ _)      = True
isSessionType _ _ (Message _ _ _)   = True
isSessionType _ _ (Choice _ _ _)    = True
isSessionType tenv kenv (Rec _ _ t) = isSessionType tenv kenv t
  -- Functional or session
isSessionType _ kenv (TypeVar p x)  = Map.member (TBind p x) kenv
  -- Type operators
isSessionType _ _ (Dualof _ _)      = True
isSessionType tenv kenv (Name p c)  = isSession $ fst $ tenv Map.! (TBind p c)
  -- Otherwise: Functional types
isSessionType _ _ _                 = False

