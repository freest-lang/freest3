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
( checkNamesClash
, checkClashes
, checkDupCons
, checkDupBind
, checkDupKBind
, binOp
, unOp
) where

import           Parse.Lexer (Position, Pos, position, defaultPos, showPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Exps (Expression(..))
import           Syntax.Types (TypeMap, KBind(..), Type)
import           Syntax.Position (Var, Bind(..))
import           Utils.Errors
import           Utils.FreestState (FreestState, addError, getVenv)
import           Data.List (nub, (\\), intercalate, find)
import qualified Data.Map.Strict as Map

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

checkDupCons :: Position a => Bind -> Map.Map Bind a -> FreestState ()
checkDupCons b m =
  case Map.lookup b m of 
    Just b' -> do
      addError (position b')
        ["Conflicting definitions for constructor", styleRed (show b),
         "\tBound at:", showPos (position b'), "\n",
         "\t         ", showPos (position b)]
    Nothing -> return ()

checkNamesClash :: Bind -> String -> FreestState ()
checkNamesClash (Bind p x) msg = do
  m <- getVenv
  let b = Bind p x
  case m Map.!? b of
    Just a  ->
      addError p [msg, "\n\t at", showPos (position a),
                       "and", showPos p]
    Nothing -> return ()

-- Verifies collisions with other datatypes and within the same datatype 
checkClashes :: Bind -> [(Bind, Type)] -> FreestState ()
checkClashes (Bind p c) bs = do
  mapM_ (\(Bind p b, _)  -> checkNamesClash (Bind p b) (err b)) bs

  let clash = bs \\ nub bs
  if not (null clash) then
     mapM_ (\(Bind p b, _) -> addError p [err b]) clash
  else
    return ()
  where
    err c = "Multiple declarations of '" ++ styleRed c ++ "'"    

type Op = String

binOp :: Expression -> Pos -> Op -> Expression -> Expression
binOp left pos op right =
  App (position left) (App (position left) (Variable pos op) left) right

unOp :: Pos -> Op -> Expression -> Expression
unOp pos op expr =
  App (position expr) (Variable pos op) expr
