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

import           Syntax.Programs (VarEnv)
import           Syntax.Types (TypeMap, KBind(..), Type)
import           Syntax.Position (Pos, Var, Bind(..), position)
import           Utils.Errors
import           Utils.FreestState (FreestState, addError, getVenv)
import           Data.List (nub, (\\), intercalate, find)
import qualified Data.Map.Strict as Map

checkParamClash :: [Bind]   -> Bind  -> FreestState [Bind]
checkParamClash ps p =
  case find (== p) ps of
    Just x -> do
      addError (position x)
                 ["Conflicting definitions for argument", styleRed $ "'" ++ show p ++ "'\n\t",
                  "Bound at:", prettyPos (position x) ++ "\n\t",
                  "          " ++ prettyPos (position p)]
      return $ ps
    Nothing -> return $ p : ps

-- Assume: m1 is a singleton map
checkLabelClash :: TypeMap -> TypeMap -> FreestState TypeMap
checkLabelClash m1 m2 = -- TODO: map position?
  if not (null common)
    then do
      let ((Bind p c), _) = Map.elemAt 0 m1
      addError p
               ["Conflicting definitions for constructor", styleRed $ c,
                "Bound at:", prettyPos p]
      return m1
    else
      return $ Map.union m1 m2
    where common = Map.intersection m1 m2


checkKBindClash :: KBind -> [KBind] -> FreestState [KBind]
checkKBindClash (KBind p x k) bs =
  case find (\(KBind _ y _) -> y == x) bs of
    Just (KBind p' _ _) -> do
      addError p'
               ["Conflicting definitions for bind", styleRed $ "'" ++ x ++ "'\n\t",
                "Bound at:", prettyPos p' ++ "\n\t",
                "          " ++ prettyPos p]
      return bs      
    Nothing -> return $ (KBind p x k) : bs

checkNamesClash :: VarEnv -> Var -> Pos -> String -> FreestState ()
checkNamesClash m x p msg = do
  let b = Bind p x
  case m Map.!? b of
    Just a  ->
      addError p [msg, "\n\t at", prettyPos (position a),
                       "and", prettyPos p]
    Nothing -> return ()

-- Verifies collisions with other datatypes and within the same datatype 
checkClashes :: Bind -> [(Bind, Type)] -> FreestState ()
checkClashes (Bind p c) bs = do
  venv <- getVenv
  mapM_ (\(Bind p b, _)  -> checkNamesClash venv b p (err b)) bs

  let clash = bs \\ nub bs
  if not (null clash) then
     mapM_ (\(Bind p b, _) -> addError p [err b]) clash
  else
    return ()
  where
    err c = "Multiple declarations of '" ++ styleRed c ++ "'"    
