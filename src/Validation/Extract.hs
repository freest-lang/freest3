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
) where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict as Map

-- | The Extract Functions

-- The output is a type equivalent from that in the input, but
-- different from Rec and from Unfold. If it is a Var, then it must
-- represent a polymorphic variable.
normalize :: Type -> Type
  -- Session types
normalize (Semi _ t u) = append (normalize t) (normalize u)
normalize (Rec _ _ t) = normalize (unfold t)
  -- Functional or session
-- normalize t@(Var p x) = do
--   getFromKenv (Bind p x) >>= \case
--     Just _    -> return t
--     Nothing -> do
--       Just (TypeScheme _ _ u) <- getFromTenv (KBind p x (top p))
--       return u
  -- Type operators
normalize (Dualof _ t) = normalize (dual t)
  -- Functional types, Skip, Message, Choice, and Var
normalize t = t

-- normType :: Type -> FreestState Type
-- normType t@(Basic _ _) = return t
-- normType t@(Fun _ _ _ _) = return t
-- normType t@(PairType _ _ _) = return t
-- normType t@(Datatype _ _) = return t
-- normType t@(Skip _) = return t
-- normType t@(Message _ _ _) = return t
-- normType t@(Choice _ _ _) = return t
-- normType t@(Rec _ _ _) = (normType . unfold) t
-- normType (Dualof _ t) = (normType . dual) t
-- -- normType t@(Var p x) = do
-- --   getFromKenv (Bind p x) >>= \case
-- --     Just0_    -> addError defaultPos ["KENV MEMBER"]  >> return t
-- --     Nothing -> do
-- --       Just (TypeScheme _ _ u) <- getFromTenv (KBind p x (top p))
-- --       return u
-- normType t@(Var p x) = do
--   getFromTenv (KBind p x (top p)) >>= \case
--     Just (TypeScheme _ _ u) -> normType u -- return u
--     Nothing                 -> return t
-- normType (Semi _ t u) = do
--   nt <- normType t
--   nu <- normType u
--   return $ append nt nu 

append :: Type -> Type -> Type
append (Skip _) t = t
append (Semi p t u) v = Semi p t (append u v)
append t v = Semi (position t) t v

-- Extracts a typescheme; gives an error if it is of form Ɐ ε ⇒ T
extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme _ [] t) = do
  addError (position t) ["Expecting a type scheme; found a type", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme _ bs t) = return (bs, t)

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun t =
  case normalize t of
    (Fun _ _ u v) -> return (u, v)
    u             ->
      let p = position u in
      addError p ["Expecting a function type; found:", styleRed $ show u] >>
      return (Basic p UnitType, Basic p UnitType)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t =
  case normalize t of
    (PairType _ u v) -> return (u, v)
    u                ->
      let p = position u in
      addError p ["Expecting a pair type; found ", styleRed $ show u] >>
      return (Basic p IntType, Basic p IntType)
      
-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t =
  case normalize t of
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
extractMessage pol msg t =
  case normalize t of
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
extractChoiceMap pol msg pos t =
  case normalize t of
    (Choice _ pol m)            -> return m
    (Semi _ (Choice _ pol m) u) -> return $ Map.map (\v -> Semi (position v) v u) m
    u                           -> do
      addError pos ["Expecting an", msg, "choice; found", styleRed $ show u]
      return Map.empty

-- Extracts a datatype from a type; gives an error if a datatype is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap t =
  case normalize t of
    (Datatype _ m) -> return m
    u              -> do
      addError (position u) ["Expecting a datatype; found", styleRed $ show u]
      return $ Map.empty

-- Extracts a constructor from a choice map; gives an error if the
-- constructor is not in the map
extractCons :: Pos -> TypeMap -> Constructor -> FreestState Type
extractCons pos tm c =
  let b = Bind defaultPos c in
  case tm Map.!? b of
    Just t -> return t
    Nothing -> do
      addError pos ["Constructor", styleRed c, "not in scope"]             
      return (Basic pos UnitType)

