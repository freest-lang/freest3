{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
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

import           Parse.Phase hiding (moduleName)
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName (mkTrue, mkFalse)
import qualified Syntax.Type as T
import           Util.Error
import           Util.State hiding (void)

import           Control.Monad.State
import           Data.List ( find )
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe )

-- Modules

modulePath :: ParseState FilePath
modulePath = fromMaybe <$> getFileName <*> getModuleName

mkSpan :: Located a => a -> ParseState Span
mkSpan a = do
  let (Span _ p1 p2) = getSpan a
  m <- modulePath
  return $ Span m p1 p2

mkSpanSpan :: (Located a, Located b) => a -> b -> ParseState Span
mkSpanSpan a b = do
  let (Span _ p1 _) = getSpan a
  let (Span _ _ p2) = getSpan b
  m <- modulePath
  return $ Span m p1 p2

mkSpanFromSpan :: Located a => Span -> a -> ParseState Span
mkSpanFromSpan (Span _ p1 _) a = do
  let (Span _ _ p2) = getSpan a
  m <- modulePath
  return $ Span m p1 p2

liftModToSpan :: Span -> ParseState Span
liftModToSpan (Span _ p1 p2) = do
  m <- modulePath
  return $ Span m p1 p2

-- Parse errors

checkDupField :: Variable -> Map.Map Variable v -> ParseState ()
checkDupField x m =
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (getSpan x) (getSpan k) x
  where
    (k,_) = Map.elemAt (Map.findIndex x m) m

checkDupCase :: Variable -> E.FieldMap -> ParseState ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (getSpan x) x

checkDupBind :: Variable -> [Variable] -> ParseState ()
checkDupBind x xs
  | isWild x = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicateVar (getSpan y) "program" x (getSpan x)
    Nothing -> return ()

-- checkDupKindBind :: Bind K.Kind a -> [Bind K.Kind a] -> FreestStateT ()
-- checkDupKindBind (Bind p x _ _) bs =
--   case find (\(Bind _ y _ _) -> y == x) bs of
--     Just (Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
--     Nothing                -> return ()

checkDupCons :: (Variable, [T.Type]) -> [(Variable, [T.Type])] -> ParseState ()
checkDupCons (x, _) xts
  | any cmp xts = addError $ DuplicateFieldInDatatype (getSpan x) x pos
  | otherwise =
      getFromSignatures x >>= \case
       Just _  -> addError $ MultipleDeclarations (getSpan x) x pos
       Nothing -> return ()
  where
    cmp (y, _) = y == x
    pos = maybe defaultSpan (getSpan . fst) (find cmp xts)

checkDupProgVarDecl :: Variable -> ParseState ()
checkDupProgVarDecl x = do
  sigs <- getSignatures
  case sigs Map.!? x of
    Just _  -> addError $ MultipleDeclarations (getSpan x) x (pos sigs)
    Nothing -> return ()
 where
    pos sigs = getSpan $ fst $ Map.elemAt (Map.findIndex x sigs) sigs

checkDupTypeDecl :: Variable -> ParseState ()
checkDupTypeDecl a = do
  tys <- getTypes
  case tys Map.!? a of
    Just _   -> addError $ MultipleTypeDecl (getSpan a) a (pos tys)-- (getSpan s)
    Nothing  -> return ()
 where
    pos tys = getSpan $ fst $ Map.elemAt (Map.findIndex a tys) tys

-- verifies if there is any duplicated var in any patern, or nested pattern
checkDupVarPats :: [E.Pattern] -> ParseState ()
checkDupVarPats ps = void $ checkDupVarPats' ps []

checkDupVarPats' :: [E.Pattern] -> [Variable] -> ParseState [Variable]
checkDupVarPats' [] vs = return vs
checkDupVarPats' ((E.PatCons _ cs):xs) vs = checkDupVarPats' cs vs >>= checkDupVarPats' xs
checkDupVarPats' ((E.PatVar  v)   :xs) vs = do
   case find clause vs of
    Nothing -> checkDupVarPats' xs (v:vs)
    Just v2 -> addError (DuplicateVar (getSpan v) "program" v2 (getSpan v2))
            >> checkDupVarPats' xs (v:vs)
  where clause v2 = not (isWild v) && v == v2

-- OPERATORS

binOp :: E.Exp -> Variable -> E.Exp -> E.Exp
binOp l op r = E.App s (E.App (getSpan l) (E.Var (getSpan op) op) l) r
  where s = Span (moduleName $ getSpan l) (startPos $ getSpan l) (endPos $ getSpan r)

unOp :: Variable -> E.Exp -> Span -> E.Exp
unOp op expr s = E.App s (E.Var (getSpan op) op) expr

leftSection :: Variable -> E.Exp -> Span -> ParseState E.Exp
leftSection op e s = do
  sigs <- getSignatures
  i <- getNextIndex
  let v = mkNewVar i (mkVar s "_x")
  let t = genFstType (sigs Map.! op)
  return $ E.Abs s Un (Bind s v t
             (E.App s (E.App s (E.Var (getSpan op) op) (E.Var (getSpan op) v)) e))
  where
    genFstType (T.Arrow _ _ t _) = t
    genFstType t = t
    
-- Datatypes

typeListsToUnArrows :: Variable -> [(Variable, [T.Type])] -> [(Variable, T.Type)]
typeListsToUnArrows a = 
  map \(c, ts) -> (c, foldr (T.Arrow (getSpan c) Un) (T.Var (getSpan a) a) ts)

insertMap :: Ord k => k -> [v] -> Map.Map k [v] -> Map.Map k [v]
insertMap = Map.insertWith (++)

condCase :: Span -> E.Exp -> E.Exp -> E.Exp -> E.Exp 
condCase s i t e = E.Case s i $ Map.fromList [(mkTrue  s, ([],t))
                                             ,(mkFalse s, ([],e))]

-- This is used for star-types only. It should not be needed at parse time. But
-- then we need star types in Syntax.Type.Type
freshTVar :: MonadState (FreestS a) m => Span -> m Variable
freshTVar p = mkVar p . ('a' :) . show <$> getNextIndex
