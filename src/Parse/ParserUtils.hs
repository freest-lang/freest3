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
, checkDupTBindK
, checkDupField 
, checkDupMatch
, binOp
, unOp
, buildFunBody
, typeListToType
, funDeclToExp
) where

import           Syntax.Programs (VarEnv)
import           Syntax.Expression (Expression(..))
import           Syntax.Schemes (TypeScheme(..))
import           Syntax.Types
import           Syntax.Bind
import           Syntax.Kinds (Multiplicity(..))
import           Parse.Lexer (Position, Pos, position, defaultPos, showPos)
import           Utils.Errors
import           Utils.FreestState
import           Data.List (nub, (\\), intercalate, find)
import           Control.Monad.State
import qualified Data.Map.Strict as Map 

checkDupField :: PBind -> TypeMap -> FreestState ()
checkDupField b m =
  when (b `Map.member` m) $
    addError (position b) ["Duplicated field name", "\n",
                           "\t In a choice type:", styleRed (show b), ": ..."]

checkDupMatch :: PBind -> Map.Map PBind a -> FreestState () 
checkDupMatch b m =
  when (b `Map.member` m) $
    addError (position b) ["Pattern match is redundant", "\n",
                           "\t In a case alternative:", styleRed (show b), "-> ..."]

checkDupBind :: PBind -> [PBind] -> FreestState ()
checkDupBind b bs =
  case find (== b) bs of
    Just b' -> do
      addError (position b')
        ["Conflicting definitions for bind", styleRed (show b), "\n",
         "\tBound at:", showPos (position b'), "\n",
         "\t         ", showPos (position b)]
    Nothing -> return ()

checkDupTBindK :: TBindK -> [TBindK] -> FreestState ()
checkDupTBindK (TBindK p x _) bs =
  case find (\(TBindK _ y _) -> y == x) bs of
    Just (TBindK p' _ _) -> do
      addError p'
        ["Conflicting definitions for bind ", styleRed x, "\n",
         "\tBound at:", showPos p', "\n",
         "\t         ", showPos p]
    Nothing -> return ()

checkDupFunSig :: PBind -> FreestState ()  
checkDupFunSig b = do
  m <- getVenv
  case m Map.!? b of
    Just a  ->
      addError (position a) ["Duplicate type signatures for function", styleRed (show b), "\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupTypeDecl :: TBind -> FreestState ()  
checkDupTypeDecl b = do
  m <- getTenv
  case m Map.!? b of
    Just (a, _) ->
      addError (position a) ["Multiple declarations of type", styleRed (show b), "\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupFunDecl :: PBind -> FreestState ()
checkDupFunDecl b = do
  m <- getEenv
  when (b `Map.member` m) $
    addError (position b) ["Multiple declarations of function", styleRed (show b)]

checkDupTypeBind :: TBind -> FreestState ()
checkDupTypeBind b = do
  m <- getTenv
  case m Map.!? b of
    Just (a, _)  ->
      addError (position a) ["Multiple declarations of type\n",
                             "\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()  

-- OPERATORS

type Op = String

binOp :: Pos -> Expression -> Op -> Expression -> Expression
binOp pos left op right =
  App (position left) (App (position left) (ProgVar pos (PVar op)) left) right

unOp :: Pos -> Op -> Expression -> Expression
unOp pos op expr =
  App (position expr) (ProgVar pos (PVar op)) expr

-- Convert a list of types and a final type constructor to a type
typeListToType:: TBind -> [(PBind, [Type])] -> [(PBind, Type)]
typeListToType (TBind p x) = map (\(b, ts) -> (b, typeToFun ts))
  where typeToFun []       = (Name p x)
        typeToFun (t : ts) = Fun (position t) Un t (typeToFun ts)

-- At parsing time we may not konw the signature for the function, so
-- we type each parameter as ()
buildFunBody :: PBind -> [PBind] -> Expression -> FreestState Expression
buildFunBody f bs e =
  getFromVenv f >>= \case
    Just s -> do
      let (TypeScheme _ _ t) = s
      return $ buildExp bs t
    Nothing -> do
      addError (position f) ["Did not find the signature of function", styleRed $ show f]
      return e
  where
    buildExp :: [PBind] -> Type -> Expression
    buildExp []     _               = e
    buildExp (b:bs) (Fun _ m t1 t2) = Lambda (position b) m b t1 (buildExp bs t2)
    buildExp (b:bs) t               = Lambda (position b) Un b (omission (position b)) (buildExp bs t)

-- At parsing time we may not konw the signature for the function, so
-- we type each parameter as ()
funDeclToExp :: [PBind] -> Expression -> Expression
funDeclToExp []     e = e
funDeclToExp (b:bs) e = Lambda p Lin b (omission p) (funDeclToExp bs e)
  where p = position b
