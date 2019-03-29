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
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import           Equivalence.TypeEquivalence
import qualified Validation.Kinding as K
import           Control.Monad.State
import           Control.Conditional ((<&&>))
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
normType (Rec _ _ t) = (normType . unfold) t
normType (Dualof _ t) = (normType . dual) t
normType t@(Var p x) = do
  getFromKenv (Bind p x) >>= \case
    Just _    -> addError defaultPos [styleRed "Just"] >> return t
    Nothing -> do
      addError defaultPos [styleRed "otherwise"]
      Just (TypeScheme _ _ t) <- getFromTenv (KBind p x (top p))
      return t
normType (Semi _ t u) = do
  nt <- normType t
  nu <- normType u
  return $ append t u 
  

append :: Type -> Type -> Type
append (Skip _) t = t
append (Semi p t u) v = Semi p t (append u v)
append t v = Semi (position t) t v


-- Extracts a typescheme; gives an error if it is on Ɐ ε ⇒ T form
extractScheme :: TypeScheme -> FreestState ([KBind], Type)
extractScheme (TypeScheme p [] t) = do
  addError (position t) ["Expecting a type scheme; found", styleRed $ show t]
  return ([], (Basic (position t) UnitType))
extractScheme (TypeScheme p bs t) = return (bs, t)

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Type -> FreestState (Type, Type)
extractFun t = do
  normType t >>= \case
    (Fun _ _ t u) -> return (t, u)
    t             -> do
      let p = position t
      addError p ["Expecting a function type; found:", styleRed $ show t]
      return (Basic p UnitType, Basic p UnitType)


-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Type -> FreestState (Type, Type)
extractPair t = do
  normType t >>= \case
    (PairType _ t u) -> return (t, u)
    t                ->
      let p = position t in
      addError p ["Expecting a pair type; found ", styleRed $ show t] >>
      return (Basic p IntType, Basic p IntType)
      
-- Extracts a basic type from a general type; gives an error if it isn't a basic
extractBasic :: Type -> FreestState BasicType
extractBasic t = do
  normType t >>= \case
    (Basic _ t) -> return t
    t           ->
      addError (position t) ["Expecting a basic type; found", styleRed $ show t] >>
      return UnitType


-- Extracts an output type from a general type; gives an error if it isn't an output

extractOutput :: Type -> FreestState (BasicType, Type)
extractOutput t =  do
  normType t >>= \case
     (Message p Out b)            -> return (b, Skip p)
     (Semi _ (Message p Out b) u) -> return (b, u)
     t                            ->
       addError (position t) ["Expecting an output type; found", styleRed $ show t] >>
       return (UnitType, Skip (position t))


-- Extracts an input type from a general type; gives an error if it isn't an input
extractInput :: Type -> FreestState (BasicType, Type)
extractInput t =  do
  normType t >>= \case
     (Message p In b)            -> return (b, Skip p)
     (Semi _ (Message p In b) u) -> return (b, u)
     t                           ->
       addError (position t) ["Expecting an input type; found", styleRed $ show t] >>
       return (UnitType, Skip (position t))


-- Extracts an internal choice from a type; gives an error if it
extractInChoice :: Type -> FreestState Type
extractInChoice t = do
  normType t >>= \case
    c@(Choice _ Internal _)   -> return c
    (Semi _ (Choice p Internal m) u) -> 
       return $ Choice p Internal (Map.map (\t -> Semi (position t) t u) m)
    t                         -> do
      addError (position t) ["Expecting an internal choice; found", styleRed $ show t]
      return $ Skip (position t)

-- Extracts a datatype from a type;
-- gives an error if a datatype is not found
extractDataTypeMap :: Type -> FreestState TypeMap
extractDataTypeMap t = do
  normType t >>= \case
    (Datatype _ m) -> return m
    t              -> do
      addError (position t) ["Expecting a datatype; found", styleRed $ show t]
      return $ Map.empty

-- Extracts an internal choice from a type; gives an error if it 
extractOutChoice :: Type -> FreestState Type
extractOutChoice t = do
  normType t >>= \case
    c@(Choice _ External _)   -> return c
    (Semi _ (Choice p External m) u) -> 
       return $ Choice p External (Map.map (\t -> Semi (position t) t u) m)
    t                         -> do
      addError (position t) ["Expecting an internal choice; found", styleRed $ show t]
      return $ Skip (position t)

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
extractCons p c t = do
  addError p ["Expecting a choice; found", styleRed $ show t]
  return (Basic p UnitType)

-- Extracts an external choice map from a type;
-- gives an error if an external choice is not found
extractEChoiceMap :: Type -> FreestState TypeMap
extractEChoiceMap t = do
  normType t >>= \case
    (Choice _ External m)            -> return m
    (Semi _ (Choice _ External m) u) -> return $ Map.map (\t -> Semi (position t) t u) m
    t                                -> do
      addError (position t) ["Expecting an external choice; found", styleRed $ show t]
      return $ Map.empty
    
  where
    extractEChoiceMap' :: Type -> FreestState TypeMap
    extractEChoiceMap' (Choice _ External m) = return m
    extractEChoiceMap' (Semi p t1 t2) = do
      t3 <- extractEChoiceMap' t1
      return $ Map.map (\t -> Semi p t t2) t3
    extractEChoiceMap' t = do
      addError (position t) ["Expecting an external choice; found", styleRed $ show t]    
      return $ Map.empty
