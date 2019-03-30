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
, checkDupKBind
, checkDupField 
, checkDupMatch
--, checkClashes 
, binOp
, unOp
) where

import           Parse.Lexer (Position, Pos, position, defaultPos, showPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Exps (Expression(..))
import           Syntax.Types (TypeMap, KBind(..), Type)
import           Syntax.Bind (Var, Bind(..))
import           Utils.Errors
import           Utils.FreestState (FreestState, addError, getVenv, getTenv, getEenv)
import           Data.List (nub, (\\), intercalate, find)
import           Control.Monad.State
import qualified Data.Map.Strict as Map 

checkDupField :: Bind -> TypeMap -> FreestState ()
checkDupField b m =
  when (b `Map.member` m) $
    addError (position b) ["Duplicated field name", "\n",
                           "\t In a choice type:", styleRed (show b), ": ..."]

checkDupMatch :: Bind -> Map.Map Bind a -> FreestState () 
checkDupMatch b m =
  when (b `Map.member` m) $
    addError (position b) ["Pattern match is redundant", "\n",
                           "\t In a case alternative:", styleRed (show b), "-> ..."]

checkDupBind :: Bind -> [Bind] -> FreestState ()
checkDupBind b bs =
  case find (== b) bs of
    Just b' -> do
      addError (position b')
        ["Conflicting definitions for bind", styleRed (show b), "\n",
         "\tBound at:", showPos (position b'), "\n",
         "\t         ", showPos (position b)]
    Nothing -> return ()

checkDupKBind :: KBind -> [KBind] -> FreestState ()
checkDupKBind (KBind p x _) bs =
  case find (\(KBind _ y _) -> y == x) bs of
    Just (KBind p' _ _) -> do
      addError p'
        ["Conflicting definitions for bind ", styleRed x, "\n",
         "\tBound at:", showPos p', "\n",
         "\t         ", showPos p]
    Nothing -> return ()

checkDupFunSig :: Bind -> FreestState ()  
checkDupFunSig b = do
  m <- getVenv
  case m Map.!? b of
    Just a  ->
      addError (position a) ["Duplicate type signatures for function", styleRed (show b),
                             "\n\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupTypeDecl :: KBind -> FreestState ()  
checkDupTypeDecl b = do
  m <- getTenv
  case m Map.!? b of
    Just a  ->
      addError (position a) ["Multiple declarations of type", styleRed (show b),
                             "\n\t Declared at:", showPos (position a), "\n",
                               "\t             ", showPos (position b)]
    Nothing -> return ()

checkDupFunDecl :: Bind -> FreestState ()
checkDupFunDecl b = do
  m <- getEenv
  when (b `Map.member` m) $
    addError (position b) ["Multiple declarations of function", styleRed (show b)]

checkDupTypeBind :: KBind -> FreestState ()
checkDupTypeBind b = do
  m <- getTenv
  case m Map.!? b of
    Just a  ->
      addError (position a) ["Multiple declarations of type",
                             "\n\t Declared at:", showPos (position a), "\n",
                             "\t             ", showPos (position b)]
    Nothing -> return ()  

-- Verifies collisions with other datatypes and within the same datatype 
{-
checkClashes :: Bind -> [(Bind, Type)] -> FreestState ()
checkClashes (Bind p c) bs = do
  mapM_ (\(Bind p b, _)  -> checkDup (Bind p b) (err b)) bs

  let clash = bs \\ nub bs
  if not (null clash) then
     mapM_ (\(Bind p b, _) -> addError p [err b]) clash
  else
    return ()
  where
    err c = "Multiple declarations of '" ++ styleRed c ++ "'"    
-}
type Op = String

binOp :: Pos -> Expression -> Op -> Expression -> Expression
binOp pos left op right =
  App (position left) (App (position left) (Variable pos op) left) right

unOp :: Pos -> Op -> Expression -> Expression
unOp pos op expr =
  App (position expr) (Variable pos op) expr
