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
module Validation.Extract where

import           Parse.Lexer (Pos, position, defaultPos)
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict as Map

-- | The Extract Functions

normType :: Type -> FreestState Type
normType t@(Basic _ _) = return t
normType t@(Fun _ _ _ _) = return t
normType t@(PairType _ _ _) = return t
normType t@(Datatype _ _) = return t
normType t@(Skip _) = return t
normType t@(Message _ _ _) = return t
normType t@(Choice _ _ _) = return t
normType t@(Rec _ _ _) = (normType . unfold) t
normType (Dualof _ t) = (normType . dual) t
-- normType t@(Var p x) = do
--   getFromKenv (Bind p x) >>= \case
--     Just0_    -> addError defaultPos ["KENV MEMBER"]  >> return t
--     Nothing -> do
--       Just (TypeScheme _ _ u) <- getFromTenv (KBind p x (top p))
--       return u
normType t@(Var p x) = do
  getFromTenv (KBind p x (top p)) >>= \case
    Just (TypeScheme _ _ u) -> normType u -- return u
    Nothing                 -> return t
normType (Semi _ t u) = do
  nt <- normType t
  nu <- normType u
  return $ append nt nu 
  

append :: Type -> Type -> Type
append (Skip _) t = t
append (Semi p t u) v = Semi p t (append u v)
append t v = Semi (position t) t v


-- Extracts a typescheme; gives an error if it is on Ɐ ε ⇒ T form
extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme _ [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme _ bs t) = return (bs, t)

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun t = do
  normType t >>= \case
    (Fun _ _ u v) -> return (u, v)
    u             -> do
      let p = position u
      addError p ["Expecting a function type; found:", styleRed $ show u]
      return (Basic p UnitType, Basic p UnitType)


-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t = do
  normType t >>= \case
    (PairType _ u v) -> return (u, v)
    u                ->
      let p = position u in
      addError p ["Expecting a pair type; found ", styleRed $ show u] >>
      return (Basic p IntType, Basic p IntType)
      
-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t = do
  normType t >>= \case
    (Basic _ b) -> return b
    u           ->
      addError (position u) ["Expecting a basic type; found", styleRed $ show u] >>
      return UnitType


-- Extracts an output type from a general type; gives an error if it isn't an output

extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput t =  do
  normType t >>= \case
     (Message p Out b)            -> return (b, Skip p)
     (Semi _ (Message _ Out b) u) -> return (b, u)
     u                            ->
       addError (position u) ["Expecting an output type; found", styleRed $ show u] >>
       return (UnitType, Skip (position u))


-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput t =  do
  normType t >>= \case
     (Message p In b)            -> return (b, Skip p)
     (Semi _ (Message _ In b) u) -> return (b, u)
     u                           ->
       addError (position u) ["Expecting an input type; found", styleRed $ show u] >>
       return (UnitType, Skip (position u))


-- Extracts an internal choice from a type; gives an error if it
extractInChoice :: Type -> FreestState Type
extractInChoice t = do
  normType t >>= \case
    c@(Choice _ Out _)   -> return c
    (Semi _ (Choice p Out m) u) -> 
       return $ Choice p Out (Map.map (\v -> Semi (position v) v u) m)
    u                         -> do
      addError (position u) ["Expecting an internal choice; found", styleRed $ show u]
      return $ Skip (position u)

{- unused
-- Extracts an external choice from a type; gives an error if it 
extractExtChoice :: Type -> FreestState Type
extractExtChoice t = do
  normType t >>= \case
    c@(Choice _ In _)   -> return c
    (Semi _ (Choice p In m) u) -> 
       return $ Choice p In (Map.map (\v -> Semi (position v) v u) m)
    u                         -> do
      addError (position u) ["Expecting an external choice; found", styleRed $ show u]
      return $ Skip (position u)
-}

-- Extracts an external choice map from a type;
-- gives an error if an external choice is not found
extractExtChoiceMap :: Type -> FreestState TypeMap
extractExtChoiceMap t = do
  normType t >>= \case
    (Choice _ In m)            -> return m
    (Semi _ (Choice _ In m) u) -> return $ Map.map (\v -> Semi (position v) v u) m
    u                                -> do
      addError (position u) ["Expecting an external choice; found", styleRed $ show u]
      return $ Map.empty



-- Extracts a datatype from a type;
-- gives an error if a datatype is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap t = do
  normType t >>= \case
    (Datatype _ m) -> return m
    u              -> do
      addError (position u) ["Expecting a datatype; found", styleRed $ show u]
      return $ Map.empty

-- Extracts a constructor from a choice map;
-- gives an error if the constructor is not found
-- TODO: Join
extractCons :: Pos -> Constructor -> Type -> FreestState Type
extractCons p c (Choice _ _ tm) =
  let b = Bind p c in
  case tm Map.!? b of
    Just t -> return t
    Nothing -> do
      addError p ["Constructor", styleRed $ "'"++c++"'", "not in scope"]             
      return (Basic p UnitType)
extractCons p _ t = do
  addError p ["Expecting a choice; found", styleRed $ show t]
  return (Basic p UnitType)

