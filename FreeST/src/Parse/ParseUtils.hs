{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{- |
Module      :  Parse.ParseUtils
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Parse.ParseUtils
  ( checkDupProgVarDecl
  , checkDupFunDecl
  , checkDupTypeDecl
  , checkDupBind
  , checkDupKindBind
  , checkDupField
  , checkDupCase
  , checkDupCons
  , binOp
  , unOp
  , typeListToType
  -- , ParseResult(..)
  , FreestStateT
  -- , thenM
  -- , returnM
  )
where


import           Control.Monad.State
import           Data.Bifunctor                 ( second )
import           Data.List                      ( find )
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import           Util.Error
import           Util.FreestState

import qualified Data.Set as Set -- TODO: tmp 

type FreestStateT = StateT FreestS (Either ErrorType)

-- Parse errors

checkDupField :: Variable -> T.TypeMap -> FreestStateT ()
checkDupField x m = 
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (pos x) x

checkDupCase :: Variable -> E.FieldMap -> FreestStateT ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (pos x) x

checkDupBind :: Variable -> [Variable] -> FreestStateT ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicatePVar (pos y) x (pos x)
    Nothing -> return ()

checkDupKindBind :: Bind K.Kind a -> [Bind K.Kind a] -> FreestStateT ()
checkDupKindBind (Bind p x _ _) bs =
  case find (\(Bind _ y _ _) -> y == x) bs of
    Just (Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
    Nothing                -> return ()

checkDupCons :: (Variable, [T.Type]) -> [(Variable, [T.Type])] -> FreestStateT ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError $ DuplicateFieldInDatatype (pos x) x
  | otherwise =
     flip (Map.!?) x . varEnv <$> get >>= \case
       Just s  -> addError $ MultipleDeclarations (pos x) x (pos s)
       Nothing -> return ()

checkDupProgVarDecl :: Variable -> FreestStateT ()
checkDupProgVarDecl x = do
  vEnv <- varEnv <$> get
  case vEnv Map.!? x of
    Just a  -> addError $ MultipleDeclarations (pos x) x (pos a)
    Nothing -> return ()


checkDupTypeDecl :: Variable -> FreestStateT ()
checkDupTypeDecl a = do
  tEnv <- typeEnv <$> get
  case tEnv Map.!? a of
    Just (_, s) -> addError $ MultipleTypeDecl (pos a) a (pos s)
    Nothing     -> return ()

checkDupFunDecl :: Variable -> FreestStateT ()
checkDupFunDecl x = do
  eEnv <- parseEnv <$> get
  case eEnv Map.!? x of
    Just e  -> addError $ MultipleFunBindings (pos x) x (pos $ snd e)
    Nothing -> return ()

-- OPERATORS

binOp :: E.Exp -> Variable -> E.Exp -> E.Exp
binOp left op = E.App (pos left) (E.App (pos left) (E.Var (pos op) op) left)

unOp :: Variable -> E.Exp -> E.Exp
unOp op expr = E.App (pos expr) (E.Var (pos op) op) expr

typeListToType :: Variable -> [(Variable, [T.Type])] -> [(Variable, T.Type)]
typeListToType a = map $ second typeToFun -- map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type

 where
  typeToFun []       = T.Var (pos a) a
  typeToFun (t : ts) = T.Arrow (pos t) Un t (typeToFun ts)
      
