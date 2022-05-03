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
--  , checkDupKindBind
  , checkDupField
  , checkDupCase
  , checkDupCons
  , binOp
  , unOp
  , typeListToType
  , FreestStateT
  )
where

import           Syntax.Base
import qualified Syntax.Expression as E
-- import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState

import           Control.Monad.State
import           Data.Bifunctor ( second )
import           Data.List ( find )
import qualified Data.Map.Strict as Map
import           Prelude hiding (span)


type FreestStateT = StateT FreestS (Either ErrorType)

-- Parse errors

checkDupField :: Variable -> T.TypeMap -> FreestStateT ()
checkDupField x m = 
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (span x) (span k) x
  where
    (k,_) = Map.elemAt (Map.findIndex x m) m

checkDupCase :: Variable -> E.FieldMap -> FreestStateT ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (span x) x

checkDupBind :: Variable -> [Variable] -> FreestStateT ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicateVar (span y) "program" x (span x)
    Nothing -> return ()

-- checkDupKindBind :: Bind K.Kind a -> [Bind K.Kind a] -> FreestStateT ()
-- checkDupKindBind (Bind p x _ _) bs =
--   case find (\(Bind _ y _ _) -> y == x) bs of
--     Just (Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
--     Nothing                -> return ()

checkDupCons :: (Variable, [T.Type]) -> [(Variable, [T.Type])] -> FreestStateT ()
checkDupCons (x, _) xts
  | any compare xts = addError $ DuplicateFieldInDatatype (span x) x pos2
  | otherwise =
     flip (Map.!?) x . varEnv <$> get >>= \case
       Just _  -> addError $ MultipleDeclarations (span x) x pos2
       Nothing -> return ()
  where
    compare = \(y, _) -> y == x
    pos2 = maybe defaultSpan (span . fst) (find compare xts)

checkDupProgVarDecl :: Variable -> FreestStateT ()
checkDupProgVarDecl x = do
  vEnv <- varEnv <$> get
  case vEnv Map.!? x of
    Just _  -> addError $ MultipleDeclarations (span x) x (pos2 vEnv)
    Nothing -> return ()
 where
    pos2 vEnv = span $ fst $ Map.elemAt (Map.findIndex x vEnv) vEnv

checkDupTypeDecl :: Variable -> FreestStateT ()
checkDupTypeDecl a = do
  tEnv <- typeEnv <$> get
  case tEnv Map.!? a of
    Just (_, s) -> addError $ MultipleTypeDecl (span a) a (pos2 tEnv)-- (span s)
    Nothing     -> return ()
 where
    pos2 tEnv = span $ fst $ Map.elemAt (Map.findIndex a tEnv) tEnv
  
checkDupFunDecl :: Variable -> FreestStateT ()
checkDupFunDecl x = do
  eEnv <- parseEnv <$> get
  case eEnv Map.!? x of
    Just e  -> addError $ MultipleFunBindings (span x) x (span $ snd e)
    Nothing -> return ()

-- OPERATORS

binOp :: E.Exp -> Variable -> E.Exp -> E.Exp
binOp l op r = E.App s (E.App (span l) (E.Var (span op) op) l) r
  where s = Span (startPos $ span l) (endPos $ span r) (defModule $ span l)

unOp :: Variable -> E.Exp -> Span -> E.Exp
unOp op expr s = E.App s (E.Var (span op) op) expr


typeListToType :: Variable -> [(Variable, [T.Type])] -> [(Variable, T.Type)]
typeListToType a = map $ second typeToFun -- map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type
 where
  typeToFun []       = T.Var (span a) a
  typeToFun (t : ts) = T.Arrow (span t) Un t (typeToFun ts)
      
