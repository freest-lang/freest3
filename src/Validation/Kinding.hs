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
( checkAgainst
, synthetize
, kinding -- deprecated, TODO: remove
, un
, lin
, kindOfType -- test
, kindOfScheme -- test
, isWellFormed -- test
) where

import           Parse.Lexer (Pos, position)
import           Syntax.Programs
import           Syntax.Expression
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import           Validation.Contractive
import           Control.Monad.State
import qualified Data.Map.Strict as Map

-- Returns the kind of a given type
synthetize :: Type -> FreestState Kind
-- Session types
synthetize (Skip p) =
  return $ Kind p Session Un
synthetize (Message p _ _) =
  return $ Kind p Session Lin
synthetize (Choice p _ m) = do
  mapM_ (checkAgainst (Kind p Session Lin)) (Map.elems m)
  return $ Kind p Session Lin
synthetize (Semi p t u) = do
  m <- checkAgainstSession t
  n <- checkAgainstSession u
  return $ Kind p Session (max m n)
-- Functional
synthetize (Basic p _) =
  return $ Kind p Functional Un
synthetize (Fun p m t u) = do
  synthetize t
  synthetize u
  return $ Kind p Functional m
synthetize (PairType _ t u) = do
  kt <- synthetize t
  ku <- synthetize u
  return $ lub kt ku
synthetize (Datatype p m) = do
  ks <- mapM synthetize (Map.elems m)
  let Kind _ _ n = maximum ks
  return $ Kind p Functional n
synthetize (Rec p x@(KBind _ _ k) t) = do
  checkContractive t
  y <- freshVar
  let b = Bind p y
  addToKenv b k
  k <- synthetize $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
  return k
-- Session or functional
synthetize (Var p v) = do
  let bind = Bind p v
  b <- kenvMember bind
  if b then
    getKind bind
  else do
    addError p ["Variable not in scope: ", styleRed v]
    let k = top p
    addToKenv bind k
    return k
-- Type operators
synthetize (Dualof p t) = do
  m <- checkAgainstSession t
  return $ Kind p Session m
--synthetize (Name p c) = do
  

-- Check whether a given type is of a session kind; issue an error if
-- not. In either case return the multiplicity of the kind of the type
checkAgainstSession :: Type -> FreestState Multiplicity
checkAgainstSession t = do
  (Kind _ k m) <- synthetize t
  when (k /= Session) $
    addError (position t) ["Expecting a session type; found", styleRed $ show t]
  return m

-- Check whether a given type has a given kind
checkAgainst :: Kind -> Type -> FreestState ()
checkAgainst k (Rec p x t) = do
  checkContractive t
  y <- freshVar
  let b = Bind p y
  addToKenv b (Kind p Session Un)
  checkAgainst k $ subs (Var p y) x t -- On the fly α-conversion
  removeFromKenv b
checkAgainst k1 t = do
  k2 <- synthetize t
  when (k2 > k1) $
    addError (position k1) ["Expecting kind", styleRed $ show k1,
                            "to be a sub-kind of", styleRed $ show k2]

-- Returns the kind of a given type scheme -- TODO: type schemes do nota have kinds
kinding :: TypeScheme -> FreestState Kind
kinding (TypeScheme _ bs t) = do
  -- TODO: addToKenv -> addBindsLToKenv
  foldM_ (\_ (KBind p x k) -> addToKenv (Bind p x) k) () bs
  synthetize t

-- Determines whether a given type is of a given multiplicity
mult :: Multiplicity -> Type -> FreestState Bool
mult m t = do
  (Kind _ _ m') <- synthetize t
  return $ m' == m
      
-- Determines whether a given type is linear or not
lin :: Type -> FreestState Bool
lin = mult Lin

-- Determines whether a given type is unrestricted or not
un :: Type -> FreestState Bool
un = mult Un

-- For TESTS only, from here on

kindOfType :: KindEnv -> Type -> Kind
kindOfType k t =
  let s = (initialState  "") in
  evalState (synthetize t) (s {kindEnv=k})

kindOfScheme :: TypeScheme -> Kind
kindOfScheme t = evalState (kinding t) (initialState "")

isWellFormed :: Type -> KindEnv -> Bool
isWellFormed t k =
  let s = initialState "" in
  let s1 = execState (synthetize t) (s {kindEnv=k}) in
    null (errors s1)
