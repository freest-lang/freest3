{- |
Module      :  Typing.Rename
Description :  Renaming expressions and types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Renames types and programs so that all bound variables are distinct, that is
bear a different (natural number).
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Typing.Rename
  ( renameProgram
  , renameVar
  , subs
  , unfold
  , renameType -- for testing
  , renameTypes -- for testing
  , Rename(..) -- for testing
  )
where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( noConstructors )
import qualified Syntax.Type as T
import qualified Typing.Substitution as Subs
-- import           Typing.Phase
import           Inference.Phase
import           Util.Error ( internalError )
import           Util.State

import           Control.Monad.State hiding (void)
import qualified Data.Map.Strict as Map

renameProgram :: InfState ()
renameProgram = do
  -- Types
  tys <- getTypes
  -- | Why do we need to rename the tenv ??
  -- tys' <- tMapM (\(k, s) -> rename Map.empty s >>= \s' -> return (k, s')) tys
  -- setTypes tys'
  -- Signatures + Definitions, together
  sigs <- getSignatures
  tMapWithKeyM_ renameFun (noConstructors tys sigs)

renameFun :: Variable -> T.Type -> InfState ()
renameFun f t = do
  rename Map.empty Map.empty t >>= addToSignatures f
  getFromDefinitions f >>= \case
    Just e  -> rename Map.empty Map.empty e >>= addToDefinitions f
    Nothing -> return ()

-- Renaming the various syntactic categories

type Substitution = Map.Map Variable Variable

class Rename t where
  rename :: MonadState (FreestS a) m =>
    Substitution -> -- For type variables, σ
    Substitution -> -- For program variables, τ
    t -> m t
-- Note: Type variables and program variables come from two different universes.
-- We would like type Λa => λa:a -> λb:a -> a, the type of the truth value true
-- in System F, to be well formed.

-- Renaming binds

-- (λ x:t -> e)
instance Rename (Bind T.Type E.Exp) where
  rename σ τ (Bind p x t e) = do
    x' <- rename σ τ x
    t' <- rename σ τ t
    e' <- rename σ (Map.insert x x' τ) e
    return $ Bind p x' t' e'

-- (∀ a:k . t) or (Λ a:k => e)
instance Rename te => Rename (Bind K.Kind te) where
  rename σ τ (Bind p a k te) = do
    a' <- rename σ τ a
    te' <- rename (Map.insert a a' σ) τ te
    return $ Bind p a' k te'

-- Renaming types

instance Rename T.Type where
  -- Functional types
  rename σ τ (T.Arrow p m t u) = T.Arrow p m <$> rename σ τ t <*> rename σ τ u
  rename σ τ (T.Labelled p s m) = T.Labelled p s <$> tMapM (rename σ τ) m
  -- Session types
  rename σ τ (T.Semi p t u) = T.Semi p <$> rename σ τ t <*> rename σ τ u
  rename σ τ (T.Message p pol t) = T.Message p pol <$> rename σ τ t
  -- Polymorphism and recursive types
  rename σ τ (T.Quant s p b) = T.Quant s p <$> rename σ τ b
  -- Without rec-cleaning
  rename σ τ (T.Rec p b) = T.Rec p <$> rename σ τ b
  -- With rec-cleaning
  -- rename σ τ (T.Rec p b@(Bind _ a _ t))
  --   | a `T.isFreeIn` t = T.Rec p <$> rename σ τ b
  --   | otherwise = rename σ τ t
  rename σ _ (T.Var p a) = return $ T.Var p (Map.findWithDefault a a σ)
  -- Type operators
  rename σ τ (T.Dualof p t@T.Var{}) = T.Dualof p <$> rename σ τ t
  rename _ _ t@T.Dualof{} = internalError "Typing.Rename.rename" t
  -- Int, Float, Char, String, Skip, End
  rename _ _ t = return t

renameType :: T.Type -> T.Type
renameType = head . renameTypes . (: [])

renameTypes :: [T.Type] -> [T.Type]
renameTypes ts =
  evalState (mapM (rename Map.empty Map.empty) ts) initialS

-- Renaming expressions

instance Rename E.Exp where
  -- Variable
  rename _ τ (E.Var p x) = return $ E.Var p (Map.findWithDefault x x τ)
  -- Abstraction intro and elim
  rename σ τ (E.Abs p m b) = E.Abs p m <$> rename σ τ b
  rename σ τ (E.App p e1 e2) = E.App p <$> rename σ τ e1 <*> rename σ τ e2
  -- Pair intro and elim
  rename σ τ (E.Pair p e1 e2) = E.Pair p <$> rename σ τ e1 <*> rename σ τ e2
  rename σ τ (E.BinLet p x y e1 e2) = do
    x'  <- rename σ τ x
    y'  <- rename σ τ y
    e1' <- rename σ τ e1
    e2' <- rename σ (Map.insert y y' (Map.insert x x' τ)) e2
    return $ E.BinLet p x' y' e1' e2'
  -- Datatype elim
  rename σ τ (E.Case p e fm) =
    E.Case p <$> rename σ τ e <*> tMapM (renameField σ τ) fm
  -- Type application & TypeAbs
  rename σ τ (E.TypeAbs p b) = E.TypeAbs p <$> rename σ τ b
  rename σ τ (E.TypeApp p e t) =
    E.TypeApp p <$> rename σ τ e <*> rename σ τ t
  -- Let
  rename σ τ (E.UnLet p x e1 e2) = do
    x'  <- rename σ τ x
    e1' <- rename σ τ e1
    e2' <- rename σ (Map.insert x x' τ) e2
    return $ E.UnLet p x' e1' e2'
  -- Otherwise: Unit, Int, Float, Char, String
  rename _ _ e = return e

renameField :: MonadState (FreestS a) m => Substitution -> Substitution -> ([Variable], E.Exp) -> m ([Variable], E.Exp)
renameField σ τ (xs, e) = do
  xs' <- mapM (rename σ τ) xs
  e'  <- rename σ (insertProgVars xs') e
  return (xs', e')
 where
  insertProgVars :: [Variable] -> Substitution
  insertProgVars xs' = foldr (uncurry Map.insert) τ (zip xs xs')

-- Renaming variables

instance Rename Variable where
  rename _ _ = renameVar

renameVar :: MonadState (FreestS a) m => Variable -> m Variable
renameVar x = do
  n <- getNextIndex
  return $ mkNewVar n x

-- Substitution and unfold, the renamed versions

-- [t/x]u, substitute t for for every free occurrence of x in u;
-- rename the resulting type
subs :: T.Type -> Variable -> T.Type -> T.Type
subs = renameType `compose3` Subs.subs
-- subs t a u = renameType $ Subs.subs t a u

-- https://gist.github.com/cscalfani/30ff149a75fc5580d1f8aec61f8e5283
compose3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
compose3 = (.) . (.) . (.)

-- Unfold a recursive type (one step only)
-- rename the resulting type
unfold :: T.Type -> T.Type
unfold = renameType . Subs.unfold
