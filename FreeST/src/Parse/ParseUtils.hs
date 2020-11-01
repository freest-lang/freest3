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
  , buildFunBody
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
import           Equivalence.Normalisation
import           Syntax.Base
import           Syntax.Expressions
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.Schemes
import           Syntax.TypeVariables
import           Syntax.Types
import           Utils.ErrorMessage
import           Utils.FreestState

-- import           Debug.Trace -- debug
-- import           Syntax.Show -- debug
-- import           Utils.PreludeLoader -- debug

thenM :: ParseResult a -> (a -> ParseResult b) -> ParseResult b
m `thenM` k = case m of
  Ok     a -> k a
  Failed e -> Failed e

returnM :: a -> ParseResult a
returnM = Ok

failM :: String -> ParseResult a
failM = Failed

catchM :: ParseResult a -> (String -> ParseResult a) -> ParseResult a
catchM m k = case m of
  Ok     a -> Ok a
  Failed e -> k e


data ParseResult a = Ok a | Failed String
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



checkDupField :: ProgVar -> TypeMap -> FreestState ()
checkDupField x m = when (x `Map.member` m) $ addError
  (position x)
  [ Error "Multiple declarations of field"
  , Error x
  , Error "\n\t in a choice type"
  ]
    -- addError (position x) ["Multiple declarations of field", styleRed (show x), "\n",
    --                        "\t in a choice type"]

checkDupCase :: ProgVar -> FieldMap -> FreestState ()
checkDupCase x m = when (x `Map.member` m) $ addError
  (position x)
  [ Error "Pattern match is redundant"
  , Error "\n\t In a case alternative:"
  , Error x
  ]
    -- addError (position x) ["Pattern match is redundant", "\n",
    --                        "\t In a case alternative:", styleRed (show x)]

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise = case find (== x) xs of
    Just y -> addError
      (position y)
      [ Error "Conflicting definitions for program variable"
      , Error x
      , Error "\n\t Bound at:"
      , Error $ show (position y)
      , Error "\n\t          "
      , Error $ show (position x)
      ]
    Nothing -> return ()

checkDupKindBind :: KindBind -> [KindBind] -> FreestState ()
checkDupKindBind (KindBind p x _) bs =
  case find (\(KindBind _ y _) -> y == x) bs of
    Just (KindBind p' _ _) -> addError
      p'
      [ Error "Conflicting definitions for type variable"
      , Error x
      , Error "\n\t Bound at: "
      , Error (show p')
      , Error "\n\t           "
      , Error (show p)
      ]
    Nothing -> return ()

checkDupCons :: (ProgVar, [Type]) -> [(ProgVar, [Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = addError
    (position x)
    [ Error "Multiple declarations of"
    , Error x
    , Error "\n\t in a datatype declaration"
    ]
  | otherwise = getFromVEnv x >>= \case
    Just s -> addError
      (position x)
      [ Error "Multiple declarations of"
      , Error x
      , Error "\n\t Declared at:"
      , Error (position x)
      , Error "\n\t             "
      , Error (position s)
      ]
                -- addError (position x) ["Multiple declarations of", styleRed (show x), "\n",
                --              "\t Declared at:", show (position x), "\n",
                --              "\t             ", show (position s)]
    Nothing -> return ()

checkDupProgVarDecl :: ProgVar -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a -> addError
      (position x)
      [ Error "Multiple declarations of"
      , Error x
      , Error "\n\t Declared at:"
      , Error (position a)
      , Error "\n\t             "
      , Error (position x)
      ]
    Nothing -> return ()


checkDupTypeDecl :: TypeVar -> FreestState ()
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) -> addError
      (position a)
      [ Error "Multiple declarations of type"
      , Error a
      , Error "\n\t Declared at:"
      , Error (position a)
      , Error "\n\t             "
      , Error (position s)
      ]
    Nothing -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getEEnv
  case eEnv Map.!? x of
    Just e -> addError
      (position x)
      [ Error "Multiple bindings for function"
      , Error x
      , Error "\n\t Declared at:"
      , Error (position x)
      , Error "\n\t             "
      , Error (position e)
      ]
    Nothing -> return ()

-- OPERATORS

binOp :: Expression -> ProgVar -> Expression -> Expression
binOp left op =
  App (position left) (App (position left) (ProgVar (position op) op) left)

unOp :: ProgVar -> Expression -> Expression
unOp op expr = App (position expr) (ProgVar (position op) op) expr

typeListToType :: TypeVar -> [(ProgVar, [Type])] -> [(ProgVar, Type)]
typeListToType a = map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type
  where
    typeToFun []     = TypeName (position a) a
    typeToFun (t:ts) = Fun (position t) Un t (typeToFun ts)

buildFunBody :: ProgVar -> [ProgVar] -> Expression -> FreestState Expression
buildFunBody f bs e = getFromVEnv f >>= \case
  Just s -> do
--    let (TypeScheme _ _ t) = s
    tEnv <- getTEnv
    return $ buildExp bs (normalise tEnv s) -- Normalisation allows type names in signatures
  Nothing -> do
    addError
      (position f)
      [ Error "The binding for function"
      , Error f
      , Error "lacks an accompanying type signature"
      ]
    return e
 where
  buildExp :: [ProgVar] -> Type -> Expression
  buildExp [] _ = e
  buildExp (b : bs) (Fun _ m t1 t2) =
    Abs (position b) m (TypeBind (position b) b t1) (buildExp bs t2)
  buildExp (b : bs) (Dualof p (Fun _ m t1 t2)) =
    Abs (position b) m (TypeBind (position b) b (Dualof p t1)) (buildExp bs (Dualof p t2))
  buildExp (b : bs) t =
    Abs (position b) Un (TypeBind (position b) b (omission (position b))) (buildExp bs t)

