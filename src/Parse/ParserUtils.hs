{- |
Module      :  Parse.ParserUtils
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Parse.ParserUtils
( checkDupFunSig
, checkDupFunDecl
, checkDupTypeDecl
, checkDupBind
, checkDupTypeVarBind
, checkDupField 
, checkDupMatch
, binOp
, unOp
, buildFunBody
, typeListToType
, funDeclToExp
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import           Utils.FreestState
import           Utils.Errors
import           Parse.Lexer (showPos)
import qualified Data.Map.Strict as Map
import           Data.List (nub, (\\), intercalate, find)
import           Control.Monad.State

checkDupField :: ProgVar -> TypeMap -> FreestState ()
checkDupField b m =
  when (b `Map.member` m) $
    addError (position b) ["Duplicated field name", "\n",
                           "\t In a choice type:", styleRed (show b), ": ..."]

checkDupMatch :: ProgVar -> Map.Map ProgVar a -> FreestState () 
checkDupMatch b m =
  when (b `Map.member` m) $
    addError (position b) ["Pattern match is redundant", "\n",
                           "\t In a case alternative:", styleRed (show b), "-> ..."]

checkDupBind :: ProgVar -> [ProgVar] -> FreestState ()
checkDupBind b bs =
  case find (== b) bs of
    Just b' -> do
      addError (position b')
        ["Conflicting definitions for bind", styleRed (show b), "\n",
         "\tBound at:", showPos (position b'), "\n",
         "\t         ", showPos (position b)]
    Nothing -> return ()

checkDupTypeVarBind :: TypeVarBind -> [TypeVarBind] -> FreestState ()
checkDupTypeVarBind (TypeVarBind p x _) bs =
  case find (\(TypeVarBind _ y _) -> y == x) bs of
    Just (TypeVarBind p' _ _) -> do
      addError p'
        ["Conflicting definitions for type variable bind ", styleRed $ show x, "\n",
         "\t Bound at:", showPos p', "\n",
         "\t          ", showPos p]
    Nothing -> return ()

checkDupFunSig :: ProgVar -> FreestState ()  
checkDupFunSig b = do
  m <- getVEnv
  case m Map.!? b of
    Just a  ->
      addError (position b) ["Duplicate signatures for function", styleRed (show b), "\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupTypeDecl :: TypeVar -> FreestState ()  
checkDupTypeDecl a = do
  m <- getTEnv
  case m Map.!? a of
    Just (b, _) ->
      addError (position a) ["Multiple declarations of type", styleRed (show b), "\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupFunDecl :: ProgVar -> FreestState ()
checkDupFunDecl x = do
  eEnv <- getEEnv
  when (x `Map.member` eEnv) $
    addError (position x) ["Multiple declarations of function", styleRed (show x)]

checkDupTypeBind :: TypeVar -> FreestState ()
checkDupTypeBind b = do
  m <- getTEnv
  case m Map.!? b of
    Just (a, _)  ->
      addError (position a) ["Multiple declarations of type\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()  

-- OPERATORS

binOp :: Expression -> ProgVar -> Expression -> Expression
binOp left op right =
  App (position left) (App (position left) (ProgVar (position op) op) left) right

unOp :: ProgVar -> Expression -> Expression
unOp op expr =
  App (position expr) (ProgVar (position op) op) expr

-- Convert a list of types and a final type constructor to a type
typeListToType:: TypeVar -> [(ProgVar, [Type])] -> [(ProgVar, Type)]
typeListToType a = map (\(x, ts) -> (x, typeToFun ts))
  where typeToFun []       = TypeName (position a) a
        typeToFun (t : ts) = Fun (position t) Un t (typeToFun ts)

buildFunBody :: ProgVar -> [ProgVar] -> Expression -> FreestState Expression
buildFunBody f bs e =
  getFromVEnv f >>= \case
    Just s -> do
      let (TypeScheme _ _ t) = s
      return $ buildExp bs t
    Nothing -> do
      addError (position f) ["Did not find the signature of function", styleRed $ show f]
      return e
  where
    buildExp :: [ProgVar] -> Type -> Expression
    buildExp []     _               = e
    buildExp (b:bs) (Fun _ m t1 t2) = Lambda (position b) m b t1 (buildExp bs t2)
    buildExp (b:bs) t               = Lambda (position b) Un b (omission (position b)) (buildExp bs t)

-- At parsing time we may not konw the signature for the function, so
-- we type each parameter as ()
funDeclToExp :: [ProgVar] -> Expression -> Expression
funDeclToExp []     e = e
funDeclToExp (b:bs) e = Lambda p Lin b (omission p) (funDeclToExp bs e)
  where p = position b
