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

{-# LANGUAGE NoMonadFailDesugaring #-}

module Validation.Extract
  ( extractFun
  , extractPair
  , extractForall
  , extractOutput
  , extractInput
  , extractOutChoiceMap
  , extractInChoiceMap
  , extractDatatypeMap
  , extractCons
  )
where

import           Syntax.Expressions
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Equivalence.Normalisation
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict               as Map
-- import           Parse.Unparser -- debug
import           Syntax.Schemes -- debug
import           Debug.Trace -- debug


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
    u             -> do
      let p = position e
      addError
        p
        [ Error "Expecting a function type for expression"
        , Error e
        , Error "\n\t                               found type"
        , Error u
        ]
      return (omission p, omission p)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Expression -> Type -> FreestState (Type, Type)
extractPair e t = do
  t' <- norm t
  case t' of
    (PairType _ u v) -> return (u, v)
    u                -> do
      let p = position u
      addError
        p
        [ Error "Expecting a pair type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return (omission p, omission p)

-- Extracts a forall from a type; gives an error if there is no forall
extractForall :: Expression -> Type -> FreestState Type
extractForall e t = do
  t' <- norm t
--  traceM $ "e: " ++ show e ++ "\tt: " ++ show t ++ "\tt': " ++ show t'
  case t' of
    u@(Forall _ _ _) -> return u
    u                -> do
--      error $ show u
      let p = position u
      addError
        p
        [ Error "Expecting a polymorphic type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
        -- TODO: return a suitable type
      return $ Forall p (KindBind p (mkVar p "_") (omission p)) (omission p)

-- Extracts a basic type from a general type; gives an error if it isn't a basic
-- Deprecated: Kind MU 
-- extractBasic :: Type -> FreestState BasicType
-- extractBasic t = do
--   t' <- norm t
--   case t' of
--     (Basic _ b) -> return b
--     u ->
--       addError (position u)
--                [Error "Expecting a basic type; found type", Error u]
--         >> return IntType

-- Extracts an output type from a general type; gives an error if it isn't an output
extractOutput :: Expression -> Type -> FreestState (Type, Type)
extractOutput = extractMessage Out "output"

-- Extracts an input type from a general type; gives an error if an input is not found
extractInput :: Expression -> Type -> FreestState (Type, Type)
extractInput = extractMessage In "input"

extractMessage
  :: Polarity -> String -> Expression -> Type -> FreestState (Type, Type)
extractMessage pol msg e t = do
  t' <- norm t
  case t' of
    u@(Message p pol' b) ->
      if pol == pol' then return (b, Skip p) else extractMessageErr msg e u
    u@(Semi _ (Message _ pol' b) v) ->
      if pol == pol' then return (b, v) else extractMessageErr msg e u
    u -> extractMessageErr msg e u
 where
  extractMessageErr
    :: String -> Expression -> Type -> FreestState (Type, Type)
  extractMessageErr msg e u = do
    addError
      (position e)
      [ Error $ "Expecting an " ++ msg ++ " type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return (UnitType (position u), Skip (position u))

-- Extracts a choice type from a general type; gives an error if a choice is not found
extractOutChoiceMap :: Expression -> Type -> FreestState TypeMap
extractOutChoiceMap = extractChoiceMap Out "external"

extractInChoiceMap :: Expression -> Type -> FreestState TypeMap
extractInChoiceMap = extractChoiceMap In "internal"

extractChoiceMap
  :: Polarity -> String -> Expression -> Type -> FreestState TypeMap
extractChoiceMap pol msg e t = do
  t' <- norm t
  case t' of
    (Choice _ pol' m) ->
      if pol == pol' then return m else extractChoiceErr msg e t
    (Semi _ (Choice _ pol' m) u) -> if pol == pol'
      then return $ Map.map (\v -> Semi (position v) v u) m
      else extractChoiceErr msg e t
    u -> extractChoiceErr msg e t
 where
  extractChoiceErr :: String -> Expression -> Type -> FreestState TypeMap
  extractChoiceErr msg e u = do
    addError
      (position e)
      [ Error $ "Expecting an " ++ msg ++ " choice type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return Map.empty

-- Extracts a datatype from a type; gives an error if a datatype is not found
extractDatatypeMap :: Expression -> Type -> FreestState TypeMap
extractDatatypeMap e t = do
  t' <- norm t
  case t' of
    (Datatype _ m) -> return m
    u              -> do
      addError
        (position e)
        [ Error "Expecting a datatype for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return Map.empty

-- Extracts a constructor from a choice map; gives an error if the
-- constructor is not in the map
extractCons :: Pos -> TypeMap -> ProgVar -> FreestState Type
extractCons p tm x = case tm Map.!? x of
  Just t  -> return t
  Nothing -> do
    addError p [Error "Constructor", Error x, Error "not in scope"]
    return $ omission p
