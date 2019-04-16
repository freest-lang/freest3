{-|
Module      :  Extract
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Extract
( extractFun
, extractPair
, extractBasic
, extractOutput
, extractInput
, extractOutChoiceMap
, extractInChoiceMap
, extractDatatypeMap
, extractCons
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Equivalence.Normalisation
import           Syntax.Show
import           Parse.Lexer (Pos, position, defaultPos)
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import           Debug.Trace

-- | The Extract Functions

norm :: Type -> FreestState Type
norm t = do
  tEnv <- getTEnv
  return $ normalise tEnv t
  -- trace ("BinLet before: " ++ show t ++ "\n" ++ show tEnv) (return ())
  -- let u = normalise tEnv t
  -- trace ("BinLet after:  " ++ show u) (return ())
  -- return u

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Expression -> Type -> FreestState (Type, Type)
extractFun e t = do
  t' <- norm t
  case t' of
    (Fun _ _ u v) -> return (u, v)
    u             ->
      let p = position u in
      addError (position e) ["Expecting a function type for expression", styleRed $ show e, "\n",
                            "\t found type", styleRed $ show u] >>
      return (Basic p UnitType, Basic p UnitType)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t = do
  t' <- norm t
  case t' of
    (PairType _ u v) -> return (u, v)
    u                ->
      let p = position u in
      addError p ["Expecting a pair type; found type", styleRed $ show u] >>
      return (Basic p IntType, Basic p IntType)
      
-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t = do
  t' <- norm t
  case t' of
    (Basic _ b) -> return b
    u           ->
      addError (position u) ["Expecting a basic type; found type", styleRed $ show u] >>
      return UnitType

-- Extracts an output type from a general type; gives an error if it isn't an output
extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput = extractMessage Out "output"

-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput = extractMessage Out "input"

extractMessage :: Polarity -> String -> Type -> FreestState (BasicType, Type)
extractMessage pol msg t = do
  t' <- norm t
  case t' of
     (Message p pol b)            -> return (b, Skip p)
     (Semi _ (Message _ pol b) u) -> return (b, u)
     u                            ->
       addError (position u) ["Expecting an", msg, "type; found type", styleRed $ show u] >>
       return (UnitType, Skip (position u))

extractOutChoiceMap :: Expression -> Type -> FreestState TypeMap
extractOutChoiceMap = extractChoiceMap Out "external"

extractInChoiceMap :: Expression -> Type -> FreestState TypeMap
extractInChoiceMap = extractChoiceMap In "internal"
      
extractChoiceMap :: Polarity -> String -> Expression -> Type -> FreestState TypeMap
extractChoiceMap pol msg e t = do
  t' <- norm t
  case t' of
    (Choice _ _ m)              -> return m
    (Semi _ (Choice _ pol m) u) -> return $ Map.map (\v -> Semi (position v) v u) m
    u                           -> do
      addError (position e) ["Expecting an", msg, "choice type for expression", styleRed $ show e, "\n",
                            "\t found type", styleRed $ show u]
      return Map.empty

-- Extracts a datatype from a type; gives an error if a datatype is not found
extractDatatypeMap :: Expression -> Type -> FreestState TypeMap
extractDatatypeMap e t = do
  t' <- norm t
  case t' of
    (Datatype _ m) -> return m
    u              -> do
      addError (position e) ["Expecting a datatype for expression", styleRed $ show e, "\n",
                             "\t found type", styleRed $ show u]
      return $ Map.empty

-- Extracts a constructor from a choice map; gives an error if the
-- constructor is not in the map
extractCons :: Pos -> TypeMap -> ProgVar -> FreestState Type
extractCons p tm x =
  case tm Map.!? x of
    Just t -> return t
    Nothing -> do
      addError p ["Constructor", styleRed (show x), "not in scope"]             
      return $ Basic p UnitType
