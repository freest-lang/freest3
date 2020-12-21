{-|
Module      :  Validation.Kinding
Description :  Check the type formation
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Kinding
  ( synthetise
  , checkAgainst
  , checkAgainstSession
  , un
  , lin
  )
where

import           Syntax.Base
import           Syntax.TypeVariable
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Validation.Contractive
import           Validation.Subkind             ( (<:), join )
import           Utils.FreestState
import           Utils.Error                    ( internalError )
import           Control.Monad                  ( unless )
import qualified Control.Monad.State           as S
import qualified Data.Map.Strict               as Map

-- Returns the kind of a given type
synthetise :: K.KindEnv -> T.Type -> FreestState K.Kind
-- Functional types
synthetise _ (T.Int p) = return $ K.Kind p K.Message Un
synthetise _ (T.Char p) = return $ K.Kind p K.Message Un
synthetise _ (T.Bool p) = return $ K.Kind p K.Message Un
synthetise _ (T.Unit p) = return $ K.Kind p K.Message Un
synthetise kEnv (T.Fun p m t u) = do
  synthetise kEnv t
  synthetise kEnv u
  return $ K.Kind p K.Top m
synthetise kEnv (T.Pair p t u) = do
  (K.Kind _ _ mt) <- synthetise kEnv t
  (K.Kind _ _ mu) <- synthetise kEnv u
  return $ K.Kind p K.Top (join mt mu)
synthetise kEnv (T.Datatype p m) = do
  ks <- tMapM (synthetise kEnv) m
  let K.Kind _ _ n = foldr1 join ks
  return $ K.Kind p K.Top n
  -- Session types
synthetise _ (T.Skip p) = return $ K.su p
synthetise kEnv (T.Semi p t u) = do
  checkAgainstSession kEnv t
  checkAgainstSession kEnv u
  return $ K.sl p
synthetise kEnv (T.Message p _ t) = do
  checkAgainst kEnv (K.ml p) t
  return $ K.sl p
synthetise kEnv (T.Choice  p _ m) = do
  tMapM_ (checkAgainst kEnv (K.sl p)) m
  return $ K.sl p
-- Session or functional
synthetise kEnv (T.Rec _ (K.Bind _ a k t)) = do
  checkContractive a t
  synthetise (Map.insert a k kEnv) t
synthetise kEnv (T.Forall _ (K.Bind _ a k t)) =
  synthetise (Map.insert a k kEnv) t
synthetise kEnv (T.Var p a) = case kEnv Map.!? a of
  Just k -> return k
  Nothing -> do
    addError p [Error "Type variable not in scope:", Error a]
    return $ omission p
-- Type operators
synthetise _ (T.Name p a) = getFromTEnv a >>= \case
  Just (k, _) -> return k
  Nothing     -> do
    addError p [Error "Type name not in scope:", Error a]
    addToTEnv a (omission p) (omission p)
    return $ omission p
synthetise _ t@T.Dualof{} = internalError "Validation.Kinding.synthetise" t

-- Check the contractivity of a given type; issue an error if not
checkContractive :: TypeVar -> T.Type -> FreestState ()
checkContractive a t = unless (contractive a t) $ addError
  (pos t)
  [Error "Type", Error t, Error "is not contractive on type variable", Error a]

-- Check whether a given type is of a session kind. In any case return
-- the multiplicity of the kind of the type
checkAgainstSession :: K.KindEnv -> T.Type -> FreestState Multiplicity
checkAgainstSession kEnv t = do
  k@(K.Kind _ p m) <- synthetise kEnv t
  S.when (p /= K.Session) $ addError
    (pos t)
    [ Error "Expecting a session type\n"
    , Error "\t found type"
    , Error t
    , Error "of kind"
    , Error k
    ]
  return m

-- Check a type against a given kind
checkAgainst :: K.KindEnv -> K.Kind -> T.Type -> FreestState ()
checkAgainst kEnv expected t = do
  actual <- synthetise kEnv t
  S.when (not (actual <: expected)) $ addError
    (pos t)
    [ Error "Couldn't match expected kind"
    , Error expected
    , Error "\n\t with actual kind"
    , Error actual
    , Error "\n\t for type"
    , Error t
    ]

-- Determine whether a given type is unrestricted
un :: T.Type -> FreestState Bool
un = mult Un

-- Determine whether a given type is linear
lin :: T.Type -> FreestState Bool
lin = mult Lin

-- Determine whether a given type is of a given multiplicity
mult :: Multiplicity -> T.Type -> FreestState Bool
mult m1 s = do
  (K.Kind _ _ m2) <- synthetise Map.empty s
  return $ m2 == m1
