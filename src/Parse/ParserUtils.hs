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

module Parse.ParserUtils where

import           Parse.Lexer (Pos, position)
import           Syntax.Programs (VarEnv)
import           Syntax.Exps (Expression(..))
import           Syntax.Types (TypeMap, KBind(..), Type)
import           Syntax.Position (Var, Bind(..))
import           Utils.Errors
import           Utils.FreestState (FreestState, addError, getVenv)
import           Data.List (nub, (\\), intercalate, find)
import qualified Data.Map.Strict as Map

checkVarClash :: Bind -> [Bind] -> FreestState ()
checkVarClash p ps =
  case find (== p) ps of
    Just x -> do
      addError (position x)
                 ["Conflicting definitions for argument", styleRed $ "'" ++ show p ++ "'\n\t",
                  "Bound at:", prettyPos (position x) ++ "\n\t",
                  "          " ++ prettyPos (position p)]
    Nothing -> return ()

-- Assume: m1 is a singleton map
checkLabelClash :: TypeMap -> TypeMap -> FreestState ()
checkLabelClash m1 m2 = -- TODO: map position?
  if not (null (Map.intersection m1 m2))
    then do
      let ((Bind p c), _) = Map.elemAt 0 m1
      addError p
               ["Conflicting definitions for constructor", styleRed c,
                "Bound at:", prettyPos p]
    else
      return ()

checkKBindClash :: KBind -> [KBind] -> FreestState ()
checkKBindClash b@(KBind p x k) bs =
  case find (\(KBind _ y _) -> y == x) bs of
    Just (KBind p' _ _) -> do
      addError p'
               ["Conflicting definitions for bind ", styleRed x ++ "\n\t",
                "Bound at:", prettyPos p' ++ "\n\t",
                "          " ++ prettyPos p]
    Nothing -> return ()

checkNamesClash :: Bind -> String -> FreestState ()
checkNamesClash (Bind p x) msg = do
  m <- getVenv
  let b = Bind p x
  case m Map.!? b of
    Just a  ->
      addError p [msg, "\n\t at", prettyPos (position a),
                       "and", prettyPos p]
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
