{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  Validation.Rename
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Validation.Rename
  ( renameState
  , renameType
  , renameTypes
  , renameVar
  , subs
  , unfold
  , isFreeIn
  , Rename(..) -- for testing only
  )
where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( noConstructors )
import qualified Syntax.Type as T
import           Util.Error ( internalError )
import           Util.State
import           Validation.Phase
import qualified Validation.Substitution as Subs

import           Control.Monad.State hiding (void)
import qualified Data.Map.Strict as Map

renameState :: TypingState ()
renameState = do
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

type Bindings = Map.Map Variable Variable

class Rename t where
  rename :: MonadState (FreestS a) m => Bindings -> Bindings -> t -> m t

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

-- instance Rename K.Kind where
--  rename bs k = return k

-- Types

instance Rename T.Type where
  -- Labelled
  rename tbs pbs (T.Labelled p s m)  = T.Labelled p s <$> tMapM (rename tbs pbs) m
  -- Functional types
  rename tbs pbs (T.Arrow p m t u)   = T.Arrow p m <$> rename tbs pbs t <*> rename tbs pbs u
  rename tbs pbs (T.Message p pol t) = T.Message p pol <$> rename tbs pbs t
  -- Session types
  rename tbs pbs (T.Semi p t u)      = T.Semi p <$> rename tbs pbs t <*> rename tbs pbs u
  -- Polymorphism
  rename tbs pbs (T.Forall p b)      = T.Forall p <$> rename tbs pbs b
  -- Functional or session
  rename tbs pbs (T.Rec    p b)
    | isProperRec b = T.Rec p <$> rename tbs pbs b
    | otherwise     = rename tbs pbs (body b)
  rename tbs _ (T.Var      a     ) = return $ T.Var (findWithDefaultVar a tbs)
  rename tbs _ (T.Dualof p (T.Var a)) =
    return $ T.Dualof p $ T.Var (findWithDefaultVar a tbs)
--rename' tbs pbs (T.CoVar    p a   ) = return $ T.CoVar p (findWithDefaultVar a tbs)
  -- Type operators
  rename _ _ t@T.Dualof{}          = internalError "Validation.Rename.rename" t
  -- Otherwise: Basic, Skip, Message, TypeName
  rename _ _ t                       = return t


-- Expressions

instance Rename E.Exp where
  -- Variable
  rename _ pbs (E.Var x) = return $ E.Var (findWithDefaultVar x pbs)
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
  -- Otherwise: Unit, Integer, Character, Boolean, Select
  rename _ _ e = return e

renameField :: MonadState (FreestS a) m => Bindings -> Bindings -> ([Variable], E.Exp) -> m ([Variable], E.Exp)
renameField tbs pbs (xs, e) = do
  xs' <- mapM (rename tbs pbs) xs
  e'  <- rename tbs (insertProgVars xs') e
  return (xs', e')
 where
  insertProgVars :: [Variable] -> Bindings
  insertProgVars xs' = foldr (uncurry insertVar) pbs (zip xs xs')

-- Program and Type variables

instance Rename Variable where
  rename _ _ = renameVar

-- Managing variables

renameVar :: MonadState (FreestS a) m => Variable -> m Variable
renameVar x = do
  n <- getNextIndex
  return $ mkNewVar n x

insertVar :: Variable -> Variable -> Bindings -> Bindings
insertVar x y = Map.insert x y

findWithDefaultVar :: Variable -> Bindings -> Variable
findWithDefaultVar x bs =
  -- mkVar (getSpan x) (Map.findWithDefault (intern x) (intern x) bs)
  Map.findWithDefault x x bs

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
subs t x u = renameType $ Subs.subs t x u

-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold = renameType . Subs.unfold 

-- Does a given bind form a proper rec?
-- Does the Bind type variable occur free the type?
isProperRec :: Bind K.Kind T.Type -> Bool
isProperRec (Bind _ x _ t) = x `isFreeIn` t

-- Does a given type variable x occur free in a type t?
-- If not, then rec x.t can be renamed to t alone.
isFreeIn :: Variable -> T.Type -> Bool
  -- Labelled
isFreeIn x (T.Labelled _ _ m) =
  Map.foldr' (\t b -> x `isFreeIn` t || b) False m
    -- Functional types
isFreeIn x (T.Arrow _ _ t u) = x `isFreeIn` t || x `isFreeIn` u
    -- Session types
isFreeIn x (T.Message _ _ t) = x `isFreeIn` t ---
isFreeIn x (T.Semi _ t u) = x `isFreeIn` t || x `isFreeIn` u
  -- Polymorphism
isFreeIn x (T.Forall _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
  -- Functional or session 
isFreeIn x (T.Rec    _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
isFreeIn x (T.Var      y             ) = x == y
  -- Type operators
isFreeIn x (T.Dualof _ t) = x `isFreeIn` t
isFreeIn _ _                             = False
