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
  , subs
  , unfold
  , isFreeIn
  , Rename(..) -- for testing only
  )
where

import           Syntax.Base
import           Syntax.Program                 ( noConstructors )
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import qualified Syntax.Expression             as E
--import           Syntax.Program
-- import           Validation.Terminated          ( terminated )
import qualified Validation.Substitution       as Subs
                                                ( subs
                                                , unfold
                                                )
import           Util.Error                     ( internalError )
import           Util.FreestState
import           Util.PreludeLoader             ( userDefined )
import qualified Data.Map.Strict               as Map
import           Control.Monad.State

renameState :: FreestState ()
renameState = do
  -- TypeVenv
  tEnv <- getTEnv
  -- | Why do we need to rename the tenv ??
  -- tEnv' <- tMapM (\(k, s) -> rename Map.empty s >>= \s' -> return (k, s')) t m Env
  -- setTEnv tEnv'

  -- VarEnv + ExpEnv, together
  vEnv <- getVEnv
  tMapWithKeyM_ renameFun (userDefined (noConstructors tEnv vEnv))

renameFun :: Variable -> T.Type -> FreestState ()
renameFun f t = do
  rename Map.empty t >>= addToVEnv f
  getFromProg f >>= \case
    Just e  -> rename Map.empty e >>= addToProg f
    Nothing -> return ()

-- Renaming the various syntactic categories

type Bindings = Map.Map String String

class Rename t where
  rename :: Bindings -> t -> FreestState t

-- Binds


instance Rename t => Rename (Bind K.Kind t) where
  rename bs (Bind p a k t) = do
    a' <- rename bs a
    t' <- rename (insertVar a a' bs) t
    return $ Bind p a' k t'


instance Rename (Bind T.Type E.Exp) where
  rename bs (Bind p x t e) = do
    x' <- rename bs x
    t' <- rename bs t
    e' <- rename (insertVar x x' bs) e
    return $ Bind p x' t' e'

-- instance Rename K.Kind where
--  rename bs k = return k

-- Types

instance Rename T.Type where
  rename = rename'
  -- rename bs t
  --   | terminated Set.empty t = return $ T.Skip (pos t)
  --   | otherwise              = rename' bs t

rename' :: Bindings -> T.Type -> FreestState T.Type
    -- Functional types
rename' bs (T.Arrow p m t u    ) = T.Arrow p m <$> rename bs t <*> rename bs u
rename' bs (T.Message p pol t) = T.Message p pol <$> rename bs t
rename' bs (T.Pair p t u     ) = T.Pair p <$> rename bs t <*> rename bs u
rename' bs (T.Variant p fm  ) = T.Variant p <$> tMapM (rename bs) fm
  -- Session types
rename' bs (T.Semi   p t   u ) = T.Semi p <$> rename bs t <*> rename bs u
rename' bs (T.Choice p pol tm) = T.Choice p pol <$> tMapM (rename bs) tm
  -- Polymorphism
rename' bs (T.Forall p b     ) = T.Forall p <$> rename bs b
  -- Functional or session
rename' bs (T.Rec    p b)
 | isProperRec b = T.Rec p <$> rename bs b
 | otherwise     = rename bs (body b)
rename' bs (T.Var    p a     ) = return $ T.Var p (findWithDefaultVar a bs)
rename' bs (T.CoVar    p a   ) = return $ T.CoVar p (findWithDefaultVar a bs)
  -- Type operators
rename' bs (T.App    p t u   ) = T.App p <$> rename bs t <*> rename bs u
rename' bs (T.Abs    p b     ) = T.Abs p <$> rename bs b
rename' _  t@T.Dualof{}        = internalError "Validation.Rename.rename" t
  -- Otherwise: Basic, Skip, Message, TypeName
rename' _  t                   = return t


-- Expressions

instance Rename E.Exp where
  -- Variable
  rename bs (E.Var p x) = return $ E.Var p (findWithDefaultVar x bs)
  -- Abstraction intro and elim
  rename bs (E.Abs p m b) = E.Abs p m <$> rename bs b
  rename bs (E.App p e1 e2) = E.App p <$> rename bs e1 <*> rename bs e2
  -- Pair intro and elim
  rename bs (E.Pair p e1 e2) = E.Pair p <$> rename bs e1 <*> rename bs e2
  rename bs (E.BinLet p x y e1 e2) = do
    x'  <- rename bs x
    y'  <- rename bs y
    e1' <- rename bs e1
    e2' <- rename (insertVar y y' (insertVar x x' bs)) e2
    return $ E.BinLet p x' y' e1' e2'
  -- Datatype elim
  rename bs (E.Case p e fm) =
    E.Case p <$> rename bs e <*> tMapM (renameField bs) fm
  -- Type application & TypeAbs
  rename bs (E.TypeAbs p b) = E.TypeAbs p <$> rename bs b
  rename bs (E.TypeApp p e t) =
    E.TypeApp p <$> rename bs e <*> rename bs t
  -- Boolean elim
  rename bs (E.Cond p e1 e2 e3) =
    E.Cond p <$> rename bs e1 <*> rename bs e2 <*> rename bs e3
  -- Let
  rename bs (E.UnLet p x e1 e2) = do
    x'  <- rename bs x
    e1' <- rename bs e1
    e2' <- rename (insertVar x x' bs) e2
    return $ E.UnLet p x' e1' e2'
  -- Session types
  rename bs (E.New p t u) = E.New p <$> rename bs t <*> rename bs u
  -- Otherwise: Unit, Integer, Character, Boolean, Select
  rename _ e = return e

renameField :: Bindings -> ([Variable], E.Exp) -> FreestState ([Variable], E.Exp)
renameField bs (xs, e) = do
  xs' <- mapM (rename bs) xs
  e'  <- rename (insertProgVars xs') e
  return (xs', e')
 where
  insertProgVars :: [Variable] -> Bindings
  insertProgVars xs' = foldr (uncurry insertVar) bs (zip xs xs')

-- Program and Type variables

instance Rename Variable where
  rename _ = renameVar

-- Managing variables

renameVar :: Variable -> FreestState Variable
renameVar x = do
  n <- getNextIndex
  return $ mkNewVar n x

insertVar :: Variable -> Variable -> Bindings -> Bindings
insertVar x y = Map.insert (intern x) (intern y)

findWithDefaultVar :: Variable -> Bindings -> Variable
findWithDefaultVar x bs =
  mkVar (pos x) (Map.findWithDefault (intern x) (intern x) bs)

-- Rename a type
renameType :: T.Type -> T.Type
renameType = head . renameTypes . (: [])

-- Rename a list of types
renameTypes :: [T.Type] -> [T.Type]
renameTypes ts =
  evalState (mapM (rename Map.empty) ts) initialState

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
    -- Functional types
isFreeIn x (T.Arrow _ _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Pair _ t u ) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Variant _ fm) =
  Map.foldr' (\t b -> b || x `isFreeIn` t) False fm
    -- Session types
isFreeIn x (T.Semi _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Choice _ _ tm) =
  Map.foldr' (\t b -> b || x `isFreeIn` t) False tm
  -- Polymorphism
isFreeIn x (T.Forall _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
  -- Functional or session 
isFreeIn x (T.Rec    _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
isFreeIn x (T.Var    _ y               ) = x == y
  -- Type operators
isFreeIn x (T.App _ t u) = x `isFreeIn` t || x `isFreeIn` u
isFreeIn x (T.Abs _ (Bind _ y _ t)) = x /= y && x `isFreeIn` t
isFreeIn x (T.Dualof    _ t               ) = x `isFreeIn` t
-- It is used during elaboration; otherwise we should
-- throw an internal error
--isFreeIn _ t@T.Dualof{} =
--  internalError "Validation.Rename.isFreeIn" t
  -- Basic, Skip, Message, TypeName
isFreeIn _ _                             = False
