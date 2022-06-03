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

-- TOOD join types and reuse function
checkDupCaseP :: Variable -> E.FieldMapP -> FreestStateT ()
checkDupCaseP x m =
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
  
checkDupFunDecl :: Variable -> FreestStateT ()
checkDupFunDecl x = do
  eEnv <- parseEnv <$> get
  case eEnv Map.!? x of
    Just e  -> addError $ MultipleFunBindings (getSpan x) x (getSpan $ snd e)
    Nothing -> return ()

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

appendMap :: Ord k => k -> [v] -> Map.Map k [v] -> Map.Map k [v]
appendMap k v = Map.insertWith add k v
  where add a b = b++a

-- TODO remove      
-- updateMapWith :: Ord k => (v -> v -> v) -> k -> v -> Map.Map k v -> Map.Map k v
-- updateMapWith f k v = Map.alter (Just . maybe v (`f` v)) k
-- appendMap' :: Ord k => k -> v -> Map.Map k [v] -> Map.Map k [v]
-- appendMap' k v = updateMapWith (++) k [v]