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
, renameList
, Rename(..) -- for testing only
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
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

renameFun :: ProgVar -> TypeScheme -> FreestState ()
renameFun f (TypeScheme p xks t) = do
  -- The function signature
  xks' <- mapM (rename Map.empty) xks
  let bs = insertBindings xks xks' Map.empty
  t' <- rename bs t
  addToVEnv f (TypeScheme p xks' t')
  -- The function body
  eEnv <- getEEnv
  getFromEEnv f >>= \case
    Just e -> do
      e' <- rename bs e
      addToEEnv f e'
    Nothing ->
      return ()
  
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
  rename bs (Choice p pol tm) = do
    tm' <- tMapM (rename bs) tm
    return $ Choice p pol tm'
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

instance Rename Expression where
  -- Variable
  rename bs (ProgVar p x) =
    return $ ProgVar p (findWithDefaultVar x bs)
  -- Abstraction intro and elim
  rename bs (Lambda p m x t e) = do
    x' <- rename bs x
    t' <- rename bs t
    e' <- rename (insertVar x x' bs) e
    return $ Lambda p m x' t' e'
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
  -- Type application
  rename bs (TypeApp p x ts) = do
    let x' = findWithDefaultVar x bs
    ts' <- mapM (rename bs) ts
    return $ TypeApp p x' ts'
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
  -- Fork
  rename bs (Fork p e) = do
    e' <- rename bs e
    return $ Fork p e'
  -- Session types
  rename bs (New p t) = do
    t' <- rename bs t
    return $ New p t'
  rename bs (Send p e) = do
    e' <- rename bs e
    return $ Send p e'
  rename bs (Receive p e) = do
    e' <- rename bs e
    return $ Receive p e'
  rename bs (Select p l e) = do
    e' <- rename bs e
    return $ Select p l e'
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
  return $ (xs', e')

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
findWithDefaultVar x bs = mkVar (position x) (Map.findWithDefault (intern x) (intern x) bs)

-- Stand alone

renameType :: Type -> Type
renameType t = head (renameList [t])

renameList :: [Type] -> [Type]
renameList ts = evalState (mapM (rename Map.empty) ts) (initialState "Renaming for QuickCheck")

