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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Rename
( renameState
, renameType
, renameTypes
, subs
, unfold
, Rename(..) -- for testing only
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Validation.Substitution as Subs (subs, unfold)
import           Validation.Terminated
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           Control.Monad.State
import           Utils.PreludeLoader (userDefined) -- debugging
import           Debug.Trace -- debugging

renameState :: FreestState ()
renameState = do
  -- TypeVenv
  tEnv <- getTEnv
  tEnv' <- tMapM (\(k, s) -> rename Map.empty s >>= \s' -> return (k, s')) tEnv
  setTEnv tEnv'
  -- VarEnv + ExpEnv, together
  vEnv <- getVEnv
  tMapWithKeyM renameFun (userDefined (noConstructors tEnv vEnv))
  return ()

-- renameFun :: ProgVar -> TypeScheme -> FreestState ()
-- renameFun f (TypeScheme p xks t) = do
  -- The function signature
  -- xks' <- mapM (rename Map.empty) xks
  -- let bs = insertBindings xks xks' Map.empty
  -- t' <- rename bs t

renameFun :: ProgVar -> Type -> FreestState ()
renameFun f t = do
  liftM (addToVEnv f) (rename Map.empty t)
  -- The function body
  getFromEEnv f >>= \case
    Just e -> do
      e' <- rename Map.empty e
      addToEEnv f e'
    Nothing ->
      return ()
  
-- Renaming the various syntactic categories

type Bindings = Map.Map String String

class Rename t where
  rename :: Bindings -> t -> FreestState t

-- Type schemes

-- instance Rename TypeScheme where
--   rename bs (TypeScheme p xks t) = do
--     xks' <- mapM (rename bs) xks
--     t' <- rename (insertBindings xks xks' bs) t
--     return $ TypeScheme p xks' t'

-- insertBindings :: [KindBind] -> [KindBind] -> Bindings -> Bindings
-- insertBindings xks xks' bs =
--   foldr (\(KindBind _ x _, KindBind _ y _) -> insertVar x y) bs (zip xks xks')

-- Types

instance Rename Type where
  rename bs t
    | terminated t = return $ Skip (pos t)
    | otherwise    = rename' bs t

rename':: Bindings -> Type -> FreestState Type
    -- Functional types
rename' bs (Fun p m t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ Fun p m t' u'
rename' bs (PairType p t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ PairType p t' u'
rename' bs (Datatype p fm) = do
  fm' <- tMapM (rename bs) fm
  return $ Datatype p fm'
  -- Session types
rename' bs (Semi p t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ Semi p t' u'
rename' bs (Choice p pol tm) = do
  tm' <- tMapM (rename bs) tm
  return $ Choice p pol tm'
  -- Polymorphism
rename' bs (Forall p (KindBind p' a k) t) = do
  a' <- rename bs a
  t' <- rename (insertVar a a' bs) t
  return $ Forall p (KindBind p' a' k) t'
  -- Functional or session
rename' bs (Rec p (KindBind p' a k) t)
  | terminated t = return $ Skip p
  | a `isFreeIn` t = do
      a' <- rename bs a
      t' <- rename (insertVar a a' bs) t
      return $ Rec p (KindBind p' a' k) t'
  | otherwise = rename bs t
rename' bs (TypeVar p a) =
  return $ TypeVar p (findWithDefaultVar a bs)
  -- Type operators
rename' bs (Dualof p t) = do
  t' <- rename bs t
  return $ Dualof p t'
  -- Otherwise: Basic, Skip, Message, TypeName
rename' _ t = return t

-- TypeVar - kind binds

instance Rename KindBind where
  rename bs (KindBind p a k) = do
    a' <- rename bs a
    return $ KindBind p a' k

-- Expressions

instance Rename Expression where
  -- Variable
  rename bs (ProgVar p x) =
    return $ ProgVar p (findWithDefaultVar x bs)
  -- Abstraction intro and elim
  rename bs (Abs p1 m (TypeBind p2 x t) e) = do
    x' <- rename bs x
    t' <- rename bs t
    e' <- rename (insertVar x x' bs) e
    return $ Abs p1 m (TypeBind p2 x' t') e'
  rename bs (App p e1 e2) = do
    e1' <- rename bs e1
    e2' <- rename bs e2
    return $ App p e1' e2'
  -- Pair intro and elim
  rename bs (Pair p e1 e2) =  do
    e1' <- rename bs e1
    e2' <- rename bs e2
    return $ Pair p e1' e2'
  rename bs (BinLet p x y e1 e2) =  do
    x' <- rename bs x
    y' <- rename bs y
    e1' <- rename bs e1
    e2' <- rename (insertVar y y' (insertVar x x' bs)) e2
    return $ BinLet p x' y' e1' e2'
  -- Datatype elim
  rename bs (Case p e fm) = do
    e' <- rename bs e
    fm' <- tMapM (renameField bs) fm
    return $ Case p e' fm'
  -- Type application & TypeAbs
  rename bs (TypeAbs p kb e) = do
    e' <- rename bs e
    return $ TypeAbs p kb e'
  rename bs (TypeApp p e t) = do
--    let x' = findWithDefaultVar x bs
    e' <- rename bs e
    t' <- rename bs t
    -- ts' <- mapM (rename bs) ts
    return $ TypeApp p e' t'
  -- Boolean elim
  rename bs (Conditional p e1 e2 e3) =  do
    e1' <- rename bs e1
    e2' <- rename bs e2
    e3' <- rename bs e3
    return $ Conditional p e1' e2' e3'
  -- Let
  rename bs (UnLet p x e1 e2) = do
    x' <- rename bs x
    e1' <- rename bs e1
    e2' <- rename (insertVar x x' bs) e2
    return $ UnLet p x' e1' e2'
  -- Session types
  rename bs (New p t u) = do
    t' <- rename bs t
    u' <- rename bs u
    return $ New p t' u'
  rename bs e@(Select _ _) = return e
  rename bs (Match p e fm) = do
    e' <- rename bs e
    fm' <- tMapM (renameField bs) fm
    return $ Match p e' fm'
  -- Otherwise: Unit, Integer, Character, Boolean
  rename _ e = return e

renameField :: Bindings -> ([ProgVar], Expression) -> FreestState ([ProgVar], Expression)
renameField bs (xs, e) = do
  xs' <- mapM (rename bs) xs
  e' <- rename (insertProgVars xs xs' bs) e
  return (xs', e')

insertProgVars :: [ProgVar] -> [ProgVar] -> Bindings -> Bindings
insertProgVars xs xs' bs = foldr(\(x, x') bs -> insertVar x x' bs) bs (zip xs xs')

-- Program variables

instance Rename ProgVar where
  rename _ = renameVar

-- Type variables

instance Rename TypeVar where
  rename _ = renameVar

-- Managing variables

renameVar :: Variable v => v -> FreestState v
renameVar x =  do
    n <- getNextIndex
    return $ mkNewVar n x

insertVar :: Variable a => a -> a -> Bindings -> Bindings
insertVar x y = Map.insert (intern x) (intern y)

findWithDefaultVar :: Variable a => a -> Bindings -> a
findWithDefaultVar x bs = mkVar (pos x) (Map.findWithDefault (intern x) (intern x) bs)

-- Rename a type
renameType :: Type -> Type
renameType = head . renameTypes . (:[])

-- Rename a list of types
renameTypes :: [Type] -> [Type]
renameTypes ts = evalState (mapM (rename Map.empty) ts) (initialState "Renaming")

-- Substitution and unfold, the renamed versions

-- [t/x]u, substitute t for for every free occurrence of x in u;
-- rename the resulting type
subs :: Type -> TypeVar -> Type -> Type
subs t x u = renameType $ Subs.subs t x u

-- Unfold a recursive type (one step only)
unfold :: Type -> Type
unfold = renameType . Subs.unfold

-- Does a given type variable x occurs free in a type t?
-- If not, then rec x.t can be renamed to t alone.
isFreeIn :: TypeVar -> Type -> Bool
    -- Functional types
isFreeIn x (Fun _ _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (PairType _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (Datatype _ fm) = Map.foldr' (\t b -> x `isFreeIn` t || b) False fm
    -- Session types
isFreeIn x (Semi _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (Choice _ pol tm) = Map.foldr' (\t b -> x `isFreeIn` t || b) False tm
  -- Polymorphism
isFreeIn x (Forall _ (KindBind _ y _) t) = x /= y && x `isFreeIn` t  
  -- Functional or session 
isFreeIn x (Rec _ (KindBind _ y _) t) = x /= y && x `isFreeIn` t
isFreeIn x (TypeVar _ y) = x == y
  -- Type operators
isFreeIn x (Dualof _ t) = x `isFreeIn` t
  -- Basic, Skip, Message, TypeName
isFreeIn _ _ = True
