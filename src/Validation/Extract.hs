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
( extractScheme
, extractFun
, extractPair
, extractBasic
, extractOutput
, extractInput
, extractOutChoiceMap
, extractInChoiceMap
, extractDataTypeMap
, extractCons
, normalise
, normaliseTS
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict as Map

-- | The Extract Functions

-- The output type is equivalent to the input type, but different from
-- Rec, Unfold, and Name. If it is a Var, then it must represent a
-- polymorphic variable (because recursion variables are bound)
normalise :: Type -> FreestState Type
  -- Session types
normalise (Semi _ t u) = do
  t' <- normalise t
  u' <- normalise u
  return $ append t' u'
normalise t@(Rec _ _ _) = normalise (unfold t)
  -- Functional or session
  -- Type operators
normalise (Name p x) = do
  tenv <- getTenv
  let (_, (TypeScheme _ [] t)) = tenv Map.! (TBind p x) -- TODO: cater for polymorphic type definitions
  normalise t
normalise (Dualof _ t) = normalise (dual t)
  -- Functional types, Skip, Message, Choice, and Var
normalise t = return t

normaliseTS :: TypeScheme -> FreestState TypeScheme
normaliseTS (TypeScheme p bs t) = normalise t >>= \t' -> return $ TypeScheme p bs t'

append :: Type -> Type -> Type -- TODO: also exits in Types.hs
append (Skip _) t = t
append (Semi p t u) v = Semi p t (append u v)
append t v = Semi (position t) t v

-- Extracts a typescheme; gives an error if it is of form Ɐ ε ⇒ T
extractScheme :: TypeScheme -> FreestState ([TBindK], Type)
extractScheme (TypeScheme _ [] t) = do
  addError (position t) ["Expecting a type scheme; found a type", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme _ bs t) = return (bs, t)

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun t = do
  t' <- normalise t
  case t' of
    (Fun _ _ u v) -> return (u, v)
    u             ->
      let p = position u in
      addError p ["Expecting a function type; found:", styleRed $ show u] >>
      return (Basic p UnitType, Basic p UnitType)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t = do
  t' <- normalise t
  case t' of
    (PairType _ u v) -> return (u, v)
    u                ->
      let p = position u in
      addError p ["Expecting a pair type; found ", styleRed $ show u] >>
      return (Basic p IntType, Basic p IntType)
      
-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t = do
  t' <- normalise t
  case t' of
    (Basic _ b) -> return b
    u           ->
      addError (position u) ["Expecting a basic type; found", styleRed $ show u] >>
      return UnitType

-- Extracts an output type from a general type; gives an error if it isn't an output
extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput = extractMessage Out "output"

-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput = extractMessage Out "input"

extractMessage :: Polarity -> String -> Type -> FreestState (BasicType, Type)
extractMessage pol msg t = do
  t' <- normalise t
  case t' of
     (Message p pol b)            -> return (b, Skip p)
     (Semi _ (Message _ pol b) u) -> return (b, u)
     u                            ->
       addError (position u) ["Expecting an", msg, "type; found", styleRed $ show u] >>
       return (UnitType, Skip (position u))

extractOutChoiceMap :: Pos -> Type -> FreestState TypeMap
extractOutChoiceMap = extractChoiceMap Out "external"

extractInChoiceMap :: Pos -> Type -> FreestState TypeMap
extractInChoiceMap = extractChoiceMap In "internal"
      
extractChoiceMap :: Polarity -> String -> Pos -> Type -> FreestState TypeMap
extractChoiceMap pol msg pos t = do
  t' <- normalise t
  case t' of
    (Choice _ pol m)            -> return m
    (Semi _ (Choice _ pol m) u) -> return $ Map.map (\v -> Semi (position v) v u) m
    u                           -> do
      addError pos ["Expecting an", msg, "choice; found", styleRed $ show u]
      return Map.empty

-- Extracts a datatype from a type; gives an error if a datatype is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap t = do
  t' <- normalise t
  case t' of
    (Datatype _ m) -> return m
    u              -> do
      addError (position u) ["Expecting a datatype; found", styleRed $ show u]
      return $ Map.empty

-- Extracts a constructor from a choice map; gives an error if the
-- constructor is not in the map
extractCons :: Pos -> TypeMap -> PVar -> FreestState Type
extractCons pos tm c =
  let b = PBind defaultPos c in
  case tm Map.!? b of
    Just t -> return t
    Nothing -> do
      addError pos ["Constructor", styleRed c, "not in scope"]             
      return $ Basic pos UnitType

