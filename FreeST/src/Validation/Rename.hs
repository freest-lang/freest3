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
  )
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Utils.Error                    ( internalError )
import           Utils.FreestState
import           Utils.PreludeLoader            ( userDefined ) -- debugging
import qualified Validation.Substitution       as Subs
                                                ( subs
                                                , unfold
                                                )
import           Validation.Terminated
-- import           Debug.Trace -- debugging

renameState :: FreestState ()
renameState = do
  -- TypeVenv
  tEnv  <- getTEnv
  tEnv' <- tMapM (\(k, s) -> rename Map.empty s >>= \s' -> return (k, s')) tEnv
  setTEnv tEnv'
  -- VarEnv + ExpEnv, together
  vEnv <- getVEnv
  tMapWithKeyM_ renameFun (userDefined (noConstructors tEnv vEnv))

-- renameFun :: ProgVar -> TypeScheme -> FreestState ()
-- renameFun f (TypeScheme p xks t) = do
  -- The function signature
  -- xks' <- mapM (rename Map.empty) xks
  -- let bs = insertBindings xks xks' Map.empty
  -- t' <- rename bs t

renameFun :: ProgVar -> T.Type -> FreestState ()
renameFun f t = do
  fmap (addToVEnv f) (rename Map.empty t)
  -- The function body
  getFromEEnv f >>= \case
    Just e -> do
      e' <- rename Map.empty e
      addToEEnv f e'
    Nothing -> return ()

-- Renaming the various syntactic categories

type Bindings = Map.Map String String

class Rename t where
  rename :: Bindings -> t -> FreestState t

-- Types

instance Rename T.Type where
  rename bs t | terminated t = return $ T.Skip (pos t)
              | otherwise    = rename' bs t

rename' :: Bindings -> T.Type -> FreestState T.Type
    -- Functional types
rename' bs (T.Fun p m t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ T.Fun p m t' u'
rename' bs (T.Pair p t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ T.Pair p t' u'
rename' bs (T.Datatype p fm) = do
  fm' <- tMapM (rename bs) fm
  return $ T.Datatype p fm'
  -- Session types
rename' bs (T.Semi p t u) = do
  t' <- rename bs t
  u' <- rename bs u
  return $ T.Semi p t' u'
rename' bs (T.Choice p pol tm) = do
  tm' <- tMapM (rename bs) tm
  return $ T.Choice p pol tm'
  -- Polymorphism
rename' bs (T.Forall p (K.Bind p' a k) t) = do
  a' <- rename bs a
  t' <- rename (insertVar a a' bs) t
  return $ T.Forall p (K.Bind p' a' k) t'
  -- Functional or session
rename' bs (T.Rec p (K.Bind p' a k) t)
  | a `isFreeIn` t = do
    a' <- rename bs a
    t' <- rename (insertVar a a' bs) t
    return $ T.Rec p (K.Bind p' a' k) t'
  | otherwise = rename bs t
rename' bs (T.Var p a)  = return $ T.Var p (findWithDefaultVar a bs)
  -- Type operators
rename' _  t@T.Dualof{} = internalError "Validation.Rename.rename" t
  -- Otherwise: Basic, Skip, Message, TypeName
rename' _  t            = return t

-- Expressions

instance Rename E.Exp where
  -- Variable
  rename bs (E.Var p x) = return $ E.Var p (findWithDefaultVar x bs)
  -- Abstraction intro and elim
  rename bs (E.Abs p1 m (T.Bind p2 x t) e) = do
    x' <- rename bs x
    t' <- rename bs t
    e' <- rename (insertVar x x' bs) e
    return $ E.Abs p1 m (T.Bind p2 x' t') e'
  rename bs (E.App p e1 e2) = do
    e1' <- rename bs e1
    e2' <- rename bs e2
    return $ E.App p e1' e2'
  -- Pair intro and elim
  rename bs (E.Pair p e1 e2) = do
    e1' <- rename bs e1
    e2' <- rename bs e2
    return $ E.Pair p e1' e2'
  rename bs (E.BinLet p x y e1 e2) = do
    x'  <- rename bs x
    y'  <- rename bs y
    e1' <- rename bs e1
    e2' <- rename (insertVar y y' (insertVar x x' bs)) e2
    return $ E.BinLet p x' y' e1' e2'
  -- Datatype elim
  rename bs (E.Case p e fm) = do
    e'  <- rename bs e
    fm' <- tMapM (renameField bs) fm
    return $ E.Case p e' fm'
  -- Type application & TypeAbs
  rename bs (E.TypeAbs p kb e) = do
    e' <- rename bs e
    return $ E.TypeAbs p kb e'
  rename bs (E.TypeApp p e t) = do
    e' <- rename bs e
    t' <- rename bs t
    return $ E.TypeApp p e' t'
  -- Boolean elim
  rename bs (E.Conditional p e1 e2 e3) = do
    e1' <- rename bs e1
    e2' <- rename bs e2
    e3' <- rename bs e3
    return $ E.Conditional p e1' e2' e3'
  -- Let
  rename bs (E.UnLet p x e1 e2) = do
    x'  <- rename bs x
    e1' <- rename bs e1
    e2' <- rename (insertVar x x' bs) e2
    return $ E.UnLet p x' e1' e2'
  -- Session types
  rename bs (E.New p t u) = do
    t' <- rename bs t
    u' <- rename bs u
    return $ E.New p t' u'
  rename bs (E.Match p e fm) = do
    e'  <- rename bs e
    fm' <- tMapM (renameField bs) fm
    return $ E.Match p e' fm'
  -- Otherwise: Unit, Integer, Character, Boolean, Select
  rename _ e = return e

renameField :: Bindings -> ([ProgVar], E.Exp) -> FreestState ([ProgVar], E.Exp)
renameField bs (xs, e) = do
  xs' <- mapM (rename bs) xs
  e'  <- rename (insertProgVars xs') e
  return (xs', e')
 where
  insertProgVars :: [ProgVar] -> Bindings
  insertProgVars xs' = foldr (uncurry insertVar) bs (zip xs xs')

-- Program variables

instance Rename ProgVar where
  rename _ = renameVar

-- Type variables

instance Rename TypeVar where
  rename _ = renameVar

-- Managing variables

renameVar :: Variable v => v -> FreestState v
renameVar x = do
  n <- getNextIndex
  return $ mkNewVar n x

insertVar :: Variable a => a -> a -> Bindings -> Bindings
insertVar x y = Map.insert (intern x) (intern y)

findWithDefaultVar :: Variable a => a -> Bindings -> a
findWithDefaultVar x bs =
  mkVar (pos x) (Map.findWithDefault (intern x) (intern x) bs)

-- Rename a type
renameType :: T.Type -> T.Type
renameType = head . renameTypes . (: [])

-- Rename a list of types
renameTypes :: [T.Type] -> [T.Type]
renameTypes ts =
  evalState (mapM (rename Map.empty) ts) (initialState "Renaming")

-- Substitution and unfold, the renamed versions

-- [t/x]u, substitute t for for every free occurrence of x in u;
-- rename the resulting type
subs :: T.Type -> TypeVar -> T.Type -> T.Type
subs t x u = renameType $ Subs.subs t x u

-- Unfold a recursive type (one step only)
unfold :: T.Type -> T.Type
unfold = renameType . Subs.unfold

-- Does a given type variable x occur free in a type t?
-- If not, then rec x.t can be renamed to t alone.
isFreeIn :: TypeVar -> T.Type -> Bool
    -- Functional types
isFreeIn x (T.Fun _ _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Pair _ t u ) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Datatype _ fm) =
  Map.foldr' (\t b -> x `isFreeIn` t || b) False fm
    -- Session types
isFreeIn x (T.Semi _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Choice _ _ tm) =
  Map.foldr' (\t b -> x `isFreeIn` t || b) False tm
  -- Polymorphism
isFreeIn x (T.Forall _ (K.Bind _ y _) t) = x /= y && x `isFreeIn` t
  -- Functional or session 
isFreeIn x (T.Rec    _ (K.Bind _ y _) t) = x /= y && x `isFreeIn` t
isFreeIn x (T.Var _ y                  ) = x == y
  -- Type operators
isFreeIn _ t@T.Dualof{} = internalError "Validation.Rename.isFreeIn" t
  -- Basic, Skip, Message, TypeName
isFreeIn _ _                             = False
