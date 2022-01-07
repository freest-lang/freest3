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

{-# LANGUAGE LambdaCase #-}

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
  , forallTypeOnCons
  , buildVariant
  , buildRecursive
  , ParseResult(..)
  , FreestStateT
  , thenM
  , returnM
  )
where


import           Control.Monad.State
import           Data.Bifunctor                 ( second )
import           Data.List                      ( find )
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.Error
import           Util.FreestState
-- import           Validation.Rename
import           Validation.Substitution

-- import qualified Control.Monad.Fail as Fail

thenM :: ParseResult a -> (a -> ParseResult b) -> ParseResult b
m `thenM` k = case m of
  Ok     a -> k a
  Failed e -> Failed e

returnM :: a -> ParseResult a
returnM = Ok

-- failM :: ErrorType -> ParseResult a
-- failM s = Failed (defaultPos, s)

-- catchM :: ParseResult a -> ((Pos, ErrorType) -> ParseResult a) -> ParseResult a
-- catchM m k = case m of
--   Ok     a -> Ok a
--   Failed e -> k e

data ParseResult a = Ok a | Failed ErrorType
type FreestStateT = StateT FreestS ParseResult

instance Monad ParseResult where
  (>>=)  = thenM
  return = returnM
--   fail   = Fail.fail

-- instance Fail.MonadFail ParseResult where
--   fail = failM

instance Applicative ParseResult where
--
  pure  = return
  (<*>) = ap

instance Functor ParseResult where
  fmap = liftM

-- Parse errors

checkDupField :: ProgVar -> T.TypeMap -> FreestState ()
checkDupField x m =
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (pos x) x

checkDupCase :: ProgVar -> E.FieldMap -> FreestState ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (pos x) x

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicatePVar (pos y) x (pos x)
    Nothing -> return ()

checkDupKindBind :: K.Bind a -> [K.Bind a] -> FreestState ()
checkDupKindBind (K.Bind p x _ _) bs =
  case find (\(K.Bind _ y _ _) -> y == x) bs of
    Just (K.Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
    Nothing                -> return ()

checkDupCons :: (ProgVar, [T.Type]) -> [(ProgVar, [T.Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError $ DuplicateFieldInDatatype (pos x) x
  | otherwise = getFromVEnv x >>= \case
      Just s  -> addError $ MultipleDeclarations (pos x) x (pos s)
      Nothing -> return ()

checkDupProgVarDecl :: ProgVar -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a  -> addError $ MultipleDeclarations (pos x) x (pos a)
    Nothing -> return ()


checkDupTypeDecl :: TypeVar -> FreestState ()
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) -> addError $ MultipleTypeDecl (pos a) a (pos s)
    Nothing     -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getPEnv
  case eEnv Map.!? x of
    Just e  -> addError $ MultipleFunBindings (pos x) x (pos $ snd e)
    Nothing -> return ()

-- OPERATORS

binOp :: E.Exp -> ProgVar -> E.Exp -> E.Exp
binOp left op = E.App (pos left) (E.App (pos left) (E.Var (pos op) op) left)

unOp :: ProgVar -> E.Exp -> E.Exp
unOp op expr = E.App (pos expr) (E.Var (pos op) op) expr


typeListToType :: TypeVar -> [(TypeVar, K.Kind)] -> [(ProgVar, [T.Type])] -> [(ProgVar, T.Type)]
typeListToType a xs = map $ second (typeToFun xs) -- map (\(x, ts) -> (x, typeToFun ts))
 where
  typeToFun [] []       = T.Var (pos a) a
  typeToFun xs []       = foldl (\t' (x, _) -> T.App (pos a) t' (T.Var (pos x) x)) (T.Var (pos a) a) xs
  typeToFun xs (t : ts) = T.Arrow (pos t) Un t (typeToFun xs ts)

-- typeListToType :: TypeVar -> [(TypeVar, K.Kind)] -> [(ProgVar, [T.Type])] -> [(ProgVar, T.Type)]
-- typeListToType a xs ys =
--   map (\(pv, ts) ->
--          (pv, foldl (\t' (x,k) -> T.Forall (pos x) (K.Bind (pos k) x k t'))
--                       (typeToFun xs ts) xs)) ys
--  where
--   typeToFun [] []       = T.Var (pos a) a
--   typeToFun xs []       = foldl (\t' (x, _) -> T.App (pos a) t' (T.Var (pos x) x)) (T.Var (pos a) a) xs
--   typeToFun xs (t : ts) = T.Arrow (pos t) Un t (typeToFun xs ts)

--  t = map (second typeToFun) ys


forallTypeOnCons :: [(TypeVar, K.Kind)] -> T.Type -> T.Type
forallTypeOnCons = flip $ foldl (\acc (v,k) -> T.Forall (pos v) $ K.Bind (pos k) v k acc)

buildVariant :: Pos -> [(TypeVar, K.Kind)] -> [(ProgVar, T.Type)] -> T.Type
-- buildVariant p binds bs = 
--         foldl (\e (v, k) ->
--               renameType (T.Abs (pos v) (K.Bind (pos v) v k e)))
--                 (T.Variant p (Map.fromList bs)) binds
buildVariant p binds bs =         
 foldl (\t (v, k) ->
      let v' = mkVar (pos v) ("0#" ++ intern v) in
        T.Abs (pos v) (K.Bind (pos v) v' k (subs (T.Var (pos v) v') v t)))
 (T.Variant p (Map.fromList bs)) binds



buildRecursive :: (TypeVar, K.Kind) -> T.Type -> T.Type
buildRecursive vk  (T.Abs p (K.Bind p' x k t)) =
  T.Abs p $ K.Bind p' x k $ buildRecursive vk t
buildRecursive (v, k) t =
  T.Rec (pos v) $ K.Bind (pos t) v k t
