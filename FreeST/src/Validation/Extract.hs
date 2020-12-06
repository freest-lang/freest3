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

import           Syntax.Expression
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import           Syntax.Base
import           Equivalence.Normalisation
import           Utils.FreestState
import qualified Data.Map.Strict               as Map
-- import           Parse.Unparser -- debug
-- import           Debug.Trace -- debug


-- | The Extract Functions

norm :: T.Type -> FreestState T.Type
norm t = do
  tEnv <- getTEnv
  return $ normalise tEnv t
  -- trace ("BinLet before: " ++ show t ++ "\n" ++ show tEnv) (return ())
  -- let u = normalise tEnv t
  -- trace ("BinLet after:  " ++ show u) (return ())
  -- return u

-- Extracts a function from a type; gives an error if there isn't a function
extractFun :: Exp -> T.Type -> FreestState (T.Type, T.Type)
extractFun e t = do
  t' <- norm t
  case t' of
    (T.Fun _ _ u v) -> return (u, v)
    u               -> do
      let p = pos e
      addError
        p
        [ Error "Expecting a function type for expression"
        , Error e
        , Error "\n\t                               found type"
        , Error u
        ]
      return (omission p, omission p)

-- Extracts a pair from a type; gives an error if there is no pair
extractPair :: Exp -> T.Type -> FreestState (T.Type, T.Type)
extractPair e t = do
  t' <- norm t
  case t' of
    (T.PairType _ u v) -> return (u, v)
    u                  -> do
      let p = pos u
      addError
        p
        [ Error "Expecting a pair type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return (omission p, omission p)

-- Extracts a forall from a type; gives an error if there is no forall
extractForall :: Exp -> T.Type -> FreestState T.Type
extractForall e t = do
  t' <- norm t
--  traceM $ "e: " ++ show e ++ "\tt: " ++ show t ++ "\tt': " ++ show t'
  case t' of
    u@T.Forall{} -> return u
    u            -> do
--      error $ show u
      let p = pos u
      addError
        p
        [ Error "Expecting a polymorphic type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
        -- TODO: return a suitable type
      return $ T.Forall p (K.Bind p (mkVar p "_") (omission p)) (omission p)

-- Extracts a basic type from a general type; gives an error if it isn't a basic
-- Deprecated: K.Kind MU 
-- extractBasic :: Type -> FreestState BasicType
-- extractBasic t = do
--   t' <- norm t
--   case t' of
--     (Basic _ b) -> return b
--     u ->
--       addError (pos u)
--                [Error "Expecting a basic type; found type", Error u]
--         >> return IntType

-- Extracts an output type from a general type; gives an error if it isn't an output
extractOutput :: Exp -> T.Type -> FreestState (T.Type, T.Type)
extractOutput = extractMessage T.Out "output"

-- Extracts an input type from a general type; gives an error if an input is not found
extractInput :: Exp -> T.Type -> FreestState (T.Type, T.Type)
extractInput = extractMessage T.In "input"

extractMessage
  :: T.Polarity -> String -> Exp -> T.Type -> FreestState (T.Type, T.Type)
extractMessage pol msg e t = do
  t' <- norm t
  case t' of
    u@(T.Message p pol' b) ->
      if pol == pol' then return (b, T.Skip p) else extractMessageErr msg e u
    u@(T.Semi _ (T.Message _ pol' b) v) ->
      if pol == pol' then return (b, v) else extractMessageErr msg e u
    u -> extractMessageErr msg e u
 where
  extractMessageErr :: String -> Exp -> T.Type -> FreestState (T.Type, T.Type)
  extractMessageErr msg e u = do
    addError
      (pos e)
      [ Error $ "Expecting an " ++ msg ++ " type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return (T.UnitType (pos u), T.Skip (pos u))

-- Extracts a choice type from a general type; gives an error if a choice is not found
extractOutChoiceMap :: Exp -> T.Type -> FreestState T.TypeMap
extractOutChoiceMap = extractChoiceMap T.Out "external"

extractInChoiceMap :: Exp -> T.Type -> FreestState T.TypeMap
extractInChoiceMap = extractChoiceMap T.In "internal"

extractChoiceMap
  :: T.Polarity -> String -> Exp -> T.Type -> FreestState T.TypeMap
extractChoiceMap pol msg e t = do
  t' <- norm t
  case t' of
    (T.Choice _ pol' m) ->
      if pol == pol' then return m else extractChoiceErr msg e t
    (T.Semi _ (T.Choice _ pol' m) u) -> if pol == pol'
      then return $ Map.map (\v -> T.Semi (pos v) v u) m
      else extractChoiceErr msg e t
    u -> extractChoiceErr msg e t
 where
  extractChoiceErr :: String -> Exp -> T.Type -> FreestState T.TypeMap
  extractChoiceErr msg e u = do
    addError
      (pos e)
      [ Error $ "Expecting an " ++ msg ++ " choice type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return Map.empty

-- Extracts a datatype from a type; gives an error if a datatype is not found
extractDatatypeMap :: Exp -> T.Type -> FreestState T.TypeMap
extractDatatypeMap e t = do
  t' <- norm t
  case t' of
    (T.Datatype _ m) -> return m
    u                -> do
      addError
        (pos e)
        [ Error "Expecting a datatype for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return Map.empty

-- Extracts a constructor from a choice map; gives an error if the
-- constructor is not in the map
extractCons :: Pos -> T.TypeMap -> ProgVar -> FreestState T.Type
extractCons p tm x = case tm Map.!? x of
  Just t  -> return t
  Nothing -> do
    addError p [Error "Constructor", Error x, Error "not in scope"]
    return $ omission p
