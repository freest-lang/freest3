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

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

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
-- , failM
  )
where


import           Control.Monad.State
import           Data.List                      ( find )
import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import           Syntax.TypeVariable
import qualified Syntax.Type                   as T
import           Util.FreestState
-- import           Debug.Trace -- debug

thenM :: ParseResult a -> (a -> ParseResult b) -> ParseResult b
m `thenM` k = case m of
  Ok     a -> k a
  Failed e -> Failed e

returnM :: a -> ParseResult a
returnM = Ok

failM :: String -> ParseResult a
failM s = Failed (defaultPos, s)

catchM :: ParseResult a -> ((Pos, String) -> ParseResult a) -> ParseResult a
catchM m k = case m of
  Ok     a -> Ok a
  Failed e -> k e


data ParseResult a = Ok a | Failed (Pos, String)
type FreestStateT = StateT FreestS ParseResult

instance Monad ParseResult where
  (>>=)  = thenM
  return = returnM
  fail   = failM

instance Applicative ParseResult where
--
  pure  = return
  (<*>) = ap

instance Functor ParseResult where
  fmap = liftM



checkDupField :: ProgVar -> T.TypeMap -> FreestState ()
checkDupField x m = when (x `Map.member` m) $ addError
  (pos x)
  [ Error "Multiple declarations of field"
  , Error x
  , Error "\n\t in a choice type"
  ]

checkDupCase :: ProgVar -> E.FieldMap -> FreestState ()
checkDupCase x m = when (x `Map.member` m) $ addError
  (pos x)
  [ Error "Pattern match is redundant"
  , Error "\n\t In a case alternative:"
  , Error x
  ]

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y -> addError
      (pos y)
      [ Error "Conflicting definitions for program variable"
      , Error x
      , Error "\n\t Bound at:"
      , Error $ show (pos y)
      , Error "\n\t          "
      , Error $ show (pos x)
      ]
    Nothing -> return ()

checkDupKindBind :: K.Bind a -> [K.Bind a] -> FreestState ()
checkDupKindBind (K.Bind p x _ _) bs =
  case find (\(K.Bind _ y _ _) -> y == x) bs of
    Just (K.Bind p' _ _ _) -> addError
      p'
      [ Error "Conflicting definitions for type variable"
      , Error x
      , Error "\n\t Bound at: "
      , Error (show p')
      , Error "\n\t           "
      , Error (show p)
      ]
    Nothing -> return ()

checkDupCons :: (ProgVar, [T.Type]) -> [(ProgVar, [T.Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError
    (pos x)
    [ Error "Multiple declarations of"
    , Error x
    , Error "\n\t in a datatype declaration"
    ]
  | otherwise = getFromVEnv x >>= \case
    Just s -> addError
      (pos x)
      [ Error "Multiple declarations of"
      , Error x
      , Error "\n\t Declared at:"
      , Error (pos x)
      , Error "\n\t             "
      , Error (pos s)
      ]
                -- addError (pos x) ["Multiple declarations of", styleRed (show x), "\n",
                --              "\t Declared at:", show (pos x), "\n",
                --              "\t             ", show (pos s)]
    Nothing -> return ()

checkDupProgVarDecl :: ProgVar -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a -> addError
      (pos x)
      [ Error "Multiple declarations of"
      , Error x
      , Error "\n\t Declared at:"
      , Error (pos a)
      , Error "\n\t             "
      , Error (pos x)
      ]
    Nothing -> return ()


checkDupTypeDecl :: TypeVar -> FreestState ()
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) -> addError
      (pos a)
      [ Error "Multiple declarations of type"
      , Error a
      , Error "\n\t Declared at:"
      , Error (pos a)
      , Error "\n\t             "
      , Error (pos s)
      ]
    Nothing -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getPEnv
  case eEnv Map.!? x of
    Just e -> addError
      (pos x)
      [ Error "Multiple bindings for function"
      , Error x
      , Error "\n\t Declared at:"
      , Error (pos x)
      , Error "\n\t             "
      , Error (pos $ snd e)
      ]
    Nothing -> return ()

-- OPERATORS

binOp :: E.Exp -> ProgVar -> E.Exp -> E.Exp
binOp left op =
  E.App (pos left) (E.App (pos left) (E.Var (pos op) op) left)

unOp :: ProgVar -> E.Exp -> E.Exp
unOp op expr = E.App (pos expr) (E.Var (pos op) op) expr

typeListToType :: TypeVar -> [(ProgVar, [T.Type])] -> [(ProgVar, T.Type)]
typeListToType a = map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type

 where
--  typeToFun []       = TypeName (pos a) a
  typeToFun []       = T.Var (pos a) a
  typeToFun (t : ts) = T.Fun (pos t) Un t (typeToFun ts)
