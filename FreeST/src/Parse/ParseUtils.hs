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
import qualified Syntax.Type                   as T
import           Util.Error
import           Util.FreestState

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

checkDupField :: Variable -> Map.Map Variable v -> FreestState ()
checkDupField x m =
  when (x `Map.member` m) $ addError $ MultipleFieldDecl (pos x) x

checkDupCase :: Variable -> E.FieldMap -> FreestState ()
checkDupCase x m =
  when (x `Map.member` m) $ addError $ RedundantPMatch (pos x) x

checkDupBind :: Variable -> [Variable] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError $ DuplicatePVar (pos y) x (pos x)
    Nothing -> return ()

checkDupKindBind :: Bind K.Kind a -> [Bind K.Kind a] -> FreestState ()
checkDupKindBind (Bind p x _ _) bs =
  case find (\(Bind _ y _ _) -> y == x) bs of
    Just (Bind p' _ _ _) -> addError $ DuplicateTVar p' x p
    Nothing                -> return ()

checkDupCons :: (Variable, [T.Type]) -> [(Variable, [T.Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError $ DuplicateFieldInDatatype (pos x) x
  | otherwise = getFromVEnv x >>= \case
      Just s  -> addError $ MultipleDeclarations (pos x) x (pos s)
      Nothing -> return ()

checkDupProgVarDecl :: Variable -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a  -> addError $ MultipleDeclarations (pos x) x (pos a)
    Nothing -> return ()


checkDupTypeDecl :: Variable -> FreestState ()
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) -> addError $ MultipleTypeDecl (pos a) a (pos s)
    Nothing     -> return ()

checkDupFunDecl :: Variable -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getPEnv
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
