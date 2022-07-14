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

module Parse.ParseUtils where

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

type FreestStateT = StateT FreestS (Either ErrorType)

-- Modules

mkSpan :: Located a => a -> FreestStateT Span
mkSpan a = do
  let (Span p1 p2 _) = getSpan a
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName
  
mkSpanSpan :: (Located a, Located b) => a -> b -> FreestStateT Span
mkSpanSpan a b = do
  let (Span p1 _ _) = getSpan a
  let (Span _ p2 _) = getSpan b
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName

mkSpanFromSpan :: Located a => Span -> a -> FreestStateT Span
mkSpanFromSpan (Span p1 _ _) a = do
  let (Span _ p2 _) = getSpan a
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName

liftModToSpan :: Span -> FreestStateT Span
liftModToSpan (Span p1 p2 _) = do
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName

-- Parse errors

checkDupField :: Variable -> T.TypeMap -> FreestStateT ()
checkDupField x m = 
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (getSpan x) (getSpan k) x
  where
    (k,_) = Map.elemAt (Map.findIndex x m) m

checkDupCase :: Variable -> E.FieldMap -> FreestStateT ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (getSpan x) x

checkDupBind :: Variable -> [Variable] -> FreestStateT ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicateVar (getSpan y) "program" x (getSpan x)
    Nothing -> return ()

-- checkDupKindBind :: Bind K.Kind a -> [Bind K.Kind a] -> FreestStateT ()
-- checkDupKindBind (Bind p x _ _) bs =
--   case find (\(Bind _ y _ _) -> y == x) bs of
--     Just (Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
--     Nothing                -> return ()

checkDupCons :: (Variable, [T.Type]) -> [(Variable, [T.Type])] -> FreestStateT ()
checkDupCons (x, _) xts
  | any compare xts = addError $ DuplicateFieldInDatatype (getSpan x) x pos
  | otherwise =
     flip (Map.!?) x . varEnv <$> get >>= \case
       Just _  -> addError $ MultipleDeclarations (getSpan x) x pos
       Nothing -> return ()
  where
    compare = \(y, _) -> y == x
    pos = maybe defaultSpan (getSpan . fst) (find compare xts)

checkDupProgVarDecl :: Variable -> FreestStateT ()
checkDupProgVarDecl x = do
  vEnv <- varEnv <$> get
  case vEnv Map.!? x of
    Just _  -> addError $ MultipleDeclarations (getSpan x) x (pos vEnv)
    Nothing -> return ()
 where
    pos vEnv = getSpan $ fst $ Map.elemAt (Map.findIndex x vEnv) vEnv

checkDupTypeDecl :: Variable -> FreestStateT ()
checkDupTypeDecl a = do
  tEnv <- typeEnv <$> get
  case tEnv Map.!? a of
    Just (_, s) -> addError $ MultipleTypeDecl (getSpan a) a (pos tEnv)-- (getSpan s)
    Nothing     -> return ()
 where
    pos tEnv = getSpan $ fst $ Map.elemAt (Map.findIndex a tEnv) tEnv

checkNumAndDup :: Variable -> [E.Pattern] -> FreestStateT ()
checkNumAndDup fn ps = checkNumArgs fn ps >> checkDupVarPats ps

-- TODOX
-- checkNumArgs (every function line has to have the same amount of variables)
checkNumArgs :: Variable -> [E.Pattern] -> FreestStateT ()
checkNumArgs fn ps = do
  env <- parseEnvPat <$> get
  -- addError $ DifNumberOfArguments (getSpan fn) fn (length ps) (0)
  case env Map.!? fn of
    Nothing -> return ()
    Just pss ->
      let lengths = map length pss in
      if any (length ps == ) lengths
        then return ()
        else addError $ DifNumberOfArguments (getSpan fn) fn (length ps) lengths

checkDupVarPats :: [E.Pattern] -> FreestStateT ()
checkDupVarPats ps = checkDupVarPats' ps []

checkDupVarPats' :: [E.Pattern] -> [Variable] -> FreestStateT ()
checkDupVarPats' [] _ = return ()
checkDupVarPats' ((E.PatChan c cs):xs) vs = checkDupVarPats' cs vs >> checkDupVarPats' xs vs
checkDupVarPats' ((E.PatCons c cs):xs) vs = checkDupVarPats' cs vs >> checkDupVarPats' xs vs
checkDupVarPats' ((E.PatVar  v)   :xs) vs = do
  case find clause vs of
    Just v2 -> addError $ DuplicateVar (getSpan v) "program" v2 (getSpan $ v2)
    Nothing -> return ()
  checkDupVarPats' xs (v:vs)
  where clause v2 = show v /= "_" -- TODOX ugly if
                 && show v == show v2

-- OPERATORS

binOp :: E.Exp -> Variable -> E.Exp -> E.Exp
binOp l op r = E.App s (E.App (getSpan l) (E.Var (getSpan op) op) l) r
  where s = Span (startPos $ getSpan l) (endPos $ getSpan r) (defModule $ getSpan l)

unOp :: Variable -> E.Exp -> Span -> E.Exp
unOp op expr s = E.App s (E.Var (getSpan op) op) expr


typeListToType :: Variable -> [(Variable, [T.Type])] -> [(Variable, T.Type)]
typeListToType a = map $ second typeToFun -- map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type
 where
  typeToFun []       = T.Var (getSpan a) a
  typeToFun (t : ts) = T.Arrow (getSpan t) Un t (typeToFun ts)

insertMap :: Ord k => k -> [v] -> Map.Map k [v] -> Map.Map k [v]
insertMap k v = Map.insertWith (++) k v