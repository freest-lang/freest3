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
, checkDupTypeVarBind
, checkDupField
, checkDupCase
, checkDupCons
, binOp
, unOp
, buildFunBody
, typeListToType
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import           Syntax.Show
import           Equivalence.Normalisation
import           Utils.FreestState
import           Utils.Errors
import qualified Data.Map.Strict as Map
import           Data.List (nub, (\\), intercalate, find)
import           Control.Monad.State
import           Debug.Trace -- debug
import           Utils.PreludeLoader -- debug

checkDupField :: ProgVar -> TypeMap -> FreestState ()
checkDupField x m =
  when (x `Map.member` m) $
    addError (position x) ["Multiple declarations of field", styleRed (show x), "\n",
                           "\t in a choice type"]

checkDupCase :: ProgVar -> FieldMap -> FreestState () 
checkDupCase x m =
  when (x `Map.member` m) $
    addError (position x) ["Pattern match is redundant", "\n",
                           "\t In a case alternative:", styleRed (show x)]

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind x xs
  | intern x == "_" = return ()
  | otherwise       = case find (== x) xs of
    Just y -> do
      addError (position y)
        ["Conflicting definitions for program variable", styleRed (show x), "\n",
         "\t Bound at:", show (position y), "\n",
         "\t          ", show (position x)]
    Nothing -> return ()

checkDupTypeVarBind :: TypeVarBind -> [TypeVarBind] -> FreestState ()
checkDupTypeVarBind (TypeVarBind p x _) bs =
  case find (\(TypeVarBind _ y _) -> y == x) bs of
    Just (TypeVarBind p' _ _) -> do
      addError p'
        ["Conflicting definitions for type variable", styleRed $ show x, "\n",
         "\t Bound at:", show p', "\n",
         "\t          ", show p]
    Nothing -> return ()

checkDupCons :: (ProgVar, [Type]) -> [(ProgVar, [Type])] -> FreestState ()
checkDupCons (x, _) xts
  | any (\(y, _) -> y == x) xts = 
      addError (position x) ["Multiple declarations of", styleRed (show x), "\n",
                             "\t in a datatype declaration"]
  | otherwise = do
      getFromVEnv x >>= \case
        Just s  ->
          addError (position x) ["Multiple declarations of", styleRed (show x), "\n",
                                 "\t Declared at:", show (position x), "\n",
                                 "\t             ", show (position s)]
        Nothing ->      
          return ()

checkDupProgVarDecl :: ProgVar -> FreestState ()
checkDupProgVarDecl x = do
  vEnv <- getVEnv
  case vEnv Map.!? x of
    Just a  ->
      addError (position x) ["Multiple declarations of", styleRed (show x), "\n",
                             "\t Declared at:", show (position a), "\n",
                             "\t             ", show (position x)]
    Nothing -> return ()

checkDupTypeDecl :: TypeVar -> FreestState ()  
checkDupTypeDecl a = do
  tEnv <- getTEnv
  case tEnv Map.!? a of
    Just (_, s) ->
      addError (position a) ["Multiple declarations of type", styleRed (show a), "\n",
                             "\t Declared at:", show (position a), "\n",
                             "\t             ", show (position s)]
    Nothing -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getEEnv
  case eEnv Map.!? x of
    Just e ->
      addError (position x) ["Multiple bindings for function", styleRed (show x), "\n",
                             "\t Declared at:", show (position x), "\n",
                             "\t             ", show (position e)]
    Nothing -> return ()

-- OPERATORS

binOp :: Expression -> ProgVar -> Expression -> Expression
binOp left op right =
  App (position left) (App (position left) (ProgVar (position op) op) left) right

unOp :: ProgVar -> Expression -> Expression
unOp op expr =
  App (position expr) (ProgVar (position op) op) expr

typeListToType :: TypeVar -> [(ProgVar, [Type])] -> [(ProgVar, Type)]
typeListToType a = map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type
  where typeToFun []       = TypeName (position a) a
        typeToFun (t : ts) = Fun (position t) Un t (typeToFun ts)

buildFunBody :: ProgVar -> [ProgVar] -> Expression -> FreestState Expression
buildFunBody f bs e =
  getFromVEnv f >>= \case
    Just s -> do
      let (TypeScheme _ _ t) = s
      tEnv <- getTEnv
      return $ buildExp bs (normalise tEnv t) -- Normalisation allows type names in signatures
    Nothing -> do
      addError (position f) ["The binding for function", styleRed $ show f,
                             "lacks an accompanying type signature"]
      return e
  where
    buildExp :: [ProgVar] -> Type -> Expression
    buildExp []     _               = e
    buildExp (b:bs) (Fun _ m t1 t2) = Lambda (position b) m b t1 (buildExp bs t2)
    buildExp (b:bs) t               = Lambda (position b) Un b (omission (position b)) (buildExp bs t)
