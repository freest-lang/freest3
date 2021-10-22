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
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
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

checkDupField :: ProgVar -> T.TypeMap -> FreestState ()
checkDupField x m =
  when (x `Map.member` m) $ let p = pos x in addError (MultipleFieldDecl p x)

checkDupCase :: ProgVar -> E.FieldMap -> FreestState ()
checkDupCase x m =
  when (x `Map.member` m) $ let p = pos x in addError (RedundantPMatch p x)

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y  -> addError (DuplicatePVar (pos y) x (pos x))
    Nothing -> return ()

checkDupKindBind :: K.Bind a -> [K.Bind a] -> FreestState ()
checkDupKindBind (K.Bind p x _ _) bs =
  case find (\(K.Bind _ y _ _) -> y == x) bs of
    Just (K.Bind p' _ _ _) -> addError (DuplicateTVar p' x p)
    Nothing                -> return ()

checkDupCons :: (ProgVar, [T.Type]) -> [(ProgVar, [T.Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError (DuplicateFieldInDatatype (pos x) x)
  | otherwise = getFromVEnv x >>= \case
    Just s  -> let p = pos x in addError (MultipleDatatypeDecl p x (pos s))
    Nothing -> return ()

checkDupProgVarDecl :: ProgVar -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a  -> let p = pos x in addError (MultipleDatatypeDecl (pos a) x p)
    Nothing -> return ()


checkDupTypeDecl :: TypeVar -> FreestState ()
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) -> addError (MultipleTypeDecl (pos a) a (pos s))
    Nothing     -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getPEnv
  case eEnv Map.!? x of
    Just e  -> addError (MultipleFunBindings (pos x) x (pos $ snd e))
    Nothing -> return ()

-- OPERATORS

binOp :: E.Exp -> ProgVar -> E.Exp -> E.Exp
binOp left op = E.App (pos left) (E.App (pos left) (E.Var (pos op) op) left)

unOp :: ProgVar -> E.Exp -> E.Exp
unOp op expr = E.App (pos expr) (E.Var (pos op) op) expr

typeListToType :: TypeVar -> [(ProgVar, [T.Type])] -> [(ProgVar, T.Type)]
typeListToType a = map $ second typeToFun -- map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type

 where
  typeToFun []       = T.Var (pos a) a
  typeToFun (t : ts) = T.Arrow (pos t) Un t (typeToFun ts)
