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
, typesToFun
) where

import           Parse.Lexer (Position, Pos, position, defaultPos, showPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Expression (Expression(..))
import           Syntax.Types (TypeMap, TBindK(..), Type(..))
import           Syntax.Bind (PBind(..), TBind(..))
import           Syntax.Kinds (Multiplicity(..))
import           Utils.Errors
import           Utils.FreestState (FreestState, addError, getVenv, getTenv, getEenv)
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
  App (position left) (App (position left) (Variable pos op) left) right

unOp :: Pos -> Op -> Expression -> Expression
unOp pos op expr =
  App (position expr) (Variable pos op) expr

-- Convert a list of types and a final type constructor to a type
typesToFun :: TBind -> [(PBind, [Type])] -> [(PBind, Type)]
typesToFun (TBind p x) = map (\(k, ts) -> (k, typeToFun (Var p x) ts))
  where
    typeToFun :: Type -> [Type] -> Type
    typeToFun = foldr (\acc t -> Fun (position t) Un t acc)

