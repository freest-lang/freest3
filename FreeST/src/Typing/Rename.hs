{- |
Module      :  Typing.Rename
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

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
import           Typing.Phase
import           Util.Error ( internalError )
import           Util.State

import           Control.Monad.State hiding (void)
import qualified Data.Map.Strict as Map

renameProgram :: TypingState ()
renameProgram = do
  -- Types
  tys <- getTypes
  -- | Why do we need to rename the tenv ??
  -- tys' <- tMapM (\(k, s) -> rename Map.empty s >>= \s' -> return (k, s')) tys
  -- setTypes tys'
  -- Signatures + Definitions, together
  sigs <- getSignatures
  tMapWithKeyM_ renameFun (noConstructors tys sigs)

renameFun :: Variable -> T.Type -> TypingState ()
renameFun f t = do
  rename Map.empty Map.empty t >>= addToSignatures f
  getFromDefinitions f >>= \case
    Just e  -> rename Map.empty Map.empty e >>= addToDefinitions f
    Nothing -> return ()

-- Renaming the various syntactic categories

type Bindings = Map.Map String Variable -- Why String and not Syntax.Base.Variable?

class Rename t where
  rename :: MonadState (FreestS a) m => Bindings -> Bindings -> t -> m t
  -- TODO: one Bindings is enough
  
-- Binds
instance Rename t => Rename (Bind K.Kind t) where
  rename tbs pbs (Bind p a k t) = do
    a' <- rename tbs pbs a
    t' <- rename (insertVar a a' tbs) pbs t
    return $ Bind p a' k t'

instance Rename (Bind T.Type E.Exp) where
  rename tbs pbs (Bind p x t e) = do
    x' <- rename tbs pbs x
    t' <- rename tbs pbs t
    e' <- rename tbs (insertVar x x' pbs) e
    return $ Bind p x' t' e'

-- Types

instance Rename T.Type where
  -- Functional types
  rename tbs pbs (T.Arrow p m t u) = T.Arrow p m <$> rename tbs pbs t <*> rename tbs pbs u
  rename tbs pbs (T.Labelled p s m) = T.Labelled p s <$> tMapM (rename tbs pbs) m
  -- Session types
  rename tbs pbs (T.Semi p t u) = T.Semi p <$> rename tbs pbs t <*> rename tbs pbs u
  rename tbs pbs (T.Message p pol t) = T.Message p pol <$> rename tbs pbs t
  -- Polymorphism and recursive types
  rename tbs pbs (T.Forall p b) = T.Forall p <$> rename tbs pbs b
  -- rename tbs pbs t@(T.Forall s1 (Bind s2 a k u)) = do
    -- let b = mkNewVar (first t) a
    -- let vb = T.Var (getSpan b) b
    -- u' <- rename tbs pbs (subs vb a u)
    -- return $ T.Forall s1 (Bind s2 b k u')
  rename tbs pbs (T.Rec p b)
    | isProperRec b = T.Rec p <$> rename tbs pbs b
    | otherwise     = rename tbs pbs (body b)
  rename tbs _ (T.Var p a) = return $ T.Var p (findWithDefaultVar a tbs)
  -- Type operators
  rename tbs pbs (T.Dualof p t@T.Var{}) = T.Dualof p <$> rename tbs pbs t
  rename _ _ t@T.Dualof{} = internalError "Typing.Rename.rename" t
  -- Int, Float, Char, String, Skip, End
  rename _ _ t = return t

-- Does a given bind form a proper rec?
-- Does the bound type variable occur free the type?
isProperRec :: Bind K.Kind T.Type -> Bool
isProperRec (Bind _ a _ t) = a `T.isFreeIn` t

-- Expressions

instance Rename E.Exp where
  -- Variable
  rename _ pbs (E.Var p x) = return $ E.Var p (findWithDefaultVar x pbs)
  -- Abstraction intro and elim
  rename tbs pbs (E.Abs p m b) = E.Abs p m <$> rename tbs pbs b
  rename tbs pbs (E.App p e1 e2) = E.App p <$> rename tbs pbs e1 <*> rename tbs pbs e2
  -- Pair intro and elim
  rename tbs pbs (E.Pair p e1 e2) = E.Pair p <$> rename tbs pbs e1 <*> rename tbs pbs e2
  rename tbs pbs (E.BinLet p x y e1 e2) = do
    x'  <- rename tbs pbs x
    y'  <- rename tbs pbs y
    e1' <- rename tbs pbs e1
    e2' <- rename tbs (insertVar y y' (insertVar x x' pbs)) e2
    return $ E.BinLet p x' y' e1' e2'
  -- Datatype elim
  rename tbs pbs (E.Case p e fm) =
    E.Case p <$> rename tbs pbs e <*> tMapM (renameField tbs pbs) fm
  -- Type application & TypeAbs
  rename tbs pbs (E.TypeAbs p b) = E.TypeAbs p <$> rename tbs pbs b
  rename tbs pbs (E.TypeApp p e t) =
    E.TypeApp p <$> rename tbs pbs e <*> rename tbs pbs t
  -- Let
  rename tbs pbs (E.UnLet p x e1 e2) = do
    x'  <- rename tbs pbs x
    e1' <- rename tbs pbs e1
    e2' <- rename tbs (insertVar x x' pbs) e2
    return $ E.UnLet p x' e1' e2'
  -- Otherwise: Unit, Int, Float, Char, String
  rename _ _ e = return e

renameField :: MonadState (FreestS a) m => Bindings -> Bindings -> ([Variable], E.Exp) -> m ([Variable], E.Exp)
renameField tbs pbs (xs, e) = do
  xs' <- mapM (rename tbs pbs) xs
  e'  <- rename tbs (insertProgVars xs') e
  return (xs', e')
 where
  insertProgVars :: [Variable] -> Bindings
  insertProgVars xs' = foldr (uncurry insertVar) pbs (zip xs xs')

-- Rename variables

instance Rename Variable where
  rename _ _ = renameVar

renameVar :: MonadState (FreestS a) m => Variable -> m Variable
renameVar x = do
  n <- getNextIndex
  return $ mkNewVar n x

insertVar :: Variable -> Variable -> Bindings -> Bindings
insertVar x = Map.insert (intern x)

findWithDefaultVar :: Variable -> Bindings -> Variable
findWithDefaultVar x = Map.findWithDefault x (intern x)

-- Rename a type
renameType :: T.Type -> T.Type
renameType = head . renameTypes . (: [])

-- Rename a list of types
renameTypes :: [T.Type] -> [T.Type]
renameTypes ts =
  evalState (mapM (rename Map.empty Map.empty) ts) initialS

-- Substitution and unfold, the renamed versions

-- [t/x]u, substitute t for for every free occurrence of x in u;
-- rename the resulting type
subs :: T.Type -> Variable -> T.Type -> T.Type
subs t a u = renameType $ Subs.subs t a u

-- Unfold a recursive type (one step only)
-- rename the resulting type
unfold :: T.Type -> T.Type
unfold = renameType . Subs.unfold
