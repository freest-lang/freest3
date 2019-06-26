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

-- import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Validation.Substitution as Subs (subs, unfold)
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
-- TODO: maybe add renameFun  
--  tMapWithKeyM renameFun (userDefined (noConstructors tEnv vEnv))
  return ()

-- renameFun :: ProgVar -> TypeScheme -> FreestState ()
-- renameFun f (TypeScheme p xks t) = do
--   -- The function signature
--   xks' <- mapM (rename Map.empty) xks
--   let bs = insertBindings xks xks' Map.empty
--   t' <- rename bs t
--   addToVEnv f (TypeScheme p xks' t')
--   -- The function body
--   eEnv <- getEEnv
--   getFromEEnv f >>= \case
--     Just e -> do
--       e' <- rename bs e
--       addToEEnv f e'
--     Nothing ->
--       return ()
  
-- Renaming the various syntactic categories

type Bindings = Map.Map String String

class Rename t where
  rename :: Bindings -> t -> FreestState t

-- Type schemes

instance Rename TypeScheme where
  rename bs (TypeScheme p xks t) = do
    xks' <- mapM (rename bs) xks
    t' <- rename (insertBindings xks xks' bs) t
    return $ TypeScheme p xks' t'

insertBindings :: [TypeVarBind] -> [TypeVarBind] -> Bindings -> Bindings
insertBindings xks xks' bs =
  foldr (\(TypeVarBind _ x _, TypeVarBind _ y _) -> insertVar x y) bs (zip xks xks')

-- Types

instance Rename Type where
    -- Functional types
  rename bs (Fun p m t u) = do
    t' <- rename bs t
    u' <- rename bs u
    return $ Fun p m t' u'
  rename bs (PairType p t u) = do
    t' <- rename bs t
    u' <- rename bs u
    return $ PairType p t' u'
  rename bs (Datatype p fm) = do
    fm' <- tMapM (rename bs) fm
    return $ Datatype p fm'
    -- Session types
  rename bs (Semi p t u) =do
    t' <- rename bs t
    u' <- rename bs u
    return $ Semi p t' u'
  rename bs (Choice p tm) = do
    tm' <- tMapM (rename bs) tm
    return $ Choice p tm'
    -- Functional or session
  rename bs (Rec p (TypeVarBind p' x k) t) = do
    x' <- rename bs x
    t' <- rename (insertVar x x' bs) t
    return $ Rec p (TypeVarBind p' x' k) t'
  rename bs (TypeVar p x) =
    return $ TypeVar p (findWithDefaultVar x bs)
    -- Type operators
  rename bs (Dualof p t) = do
    t' <- rename bs t
    return $ Dualof p t'
    -- Otherwise: Basic, Skip, Message, TypeName
  rename _ t = return t

-- Type-kind binds

instance Rename TypeVarBind where
  rename bs (TypeVarBind p x k) = do
    y <- rename bs x
    return $ TypeVarBind p y k

-- Expressions

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
findWithDefaultVar x bs = mkVar (position x) (Map.findWithDefault (intern x) (intern x) bs)

-- Substitution and unfold, the renamed versions

-- [t/x]u, substitute t for for every free occurrence of x in u
subs :: Type -> TypeVar -> Type -> Type
subs t x u = renameType $ Subs.subs t x u

-- Unfold a recursive type (one step only)
unfold :: Type -> Type
unfold = renameType . Subs.unfold
-- unfold t@(Rec _ (TypeVarBind _ x _) u) = Subs.subs t x (renameType u)

-- Stand alone

renameType :: Type -> Type
renameType = head . renameTypes . (:[])

renameTypes :: [Type] -> [Type]
renameTypes ts = evalState (mapM (rename Map.empty) ts) (initialState "Renaming for QuickCheck")

