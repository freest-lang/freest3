{-|
Module      :  Validation.Extract
Description :  The various extract functions
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Each function normalises the type, checks whether it is of the right form and
issues an error if not.
-}

{-# LANGUAGE NoMonadFailDesugaring #-}

module Validation.Extract
  ( function
  , pair
  , forall
  , output
  , input
  , outChoiceMap
  , inChoiceMap
  , datatypeMap
  , constructor
  )
where

import           Syntax.Base
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import qualified Syntax.Expression             as E
import           Equivalence.Normalisation      ( normalise )
import           Utils.FreestState
import qualified Data.Map.Strict               as Map

function :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
function e t =
  case normalise t of
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

pair :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
pair e t =
  case normalise t of
    (T.Pair _ u v) -> return (u, v)
    u              -> do
      let p = pos u
      addError
        p
        [ Error "Expecting a pair type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return (omission p, omission p)

forall :: E.Exp -> T.Type -> FreestState T.Type
forall e t =
  case normalise t of
    u@T.Forall{} -> return u
    u            -> do
      let p = pos u
      addError
        p
        [ Error "Expecting a polymorphic type for expression"
        , Error e
        , Error "\n\t found type"
        , Error u
        ]
      return $ T.Forall p (omission p)

output :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
output = message T.Out "output"

input :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
input = message T.In "input"

message :: T.Polarity -> String -> E.Exp -> T.Type -> FreestState (T.Type, T.Type)
message pol msg e t = do
  case normalise t of
    u@(T.Message p pol' b) ->
      if pol == pol' then return (b, T.Skip p) else messageErr u
    u@(T.Semi _ (T.Message _ pol' b) v) ->
      if pol == pol' then return (b, v) else messageErr u
    u -> messageErr u
 where
  messageErr :: T.Type -> FreestState (T.Type, T.Type)
  messageErr u = do
    addError
      (pos e)
      [ Error $ "Expecting an " ++ msg ++ " type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return (T.Unit (pos u), T.Skip (pos u))

outChoiceMap :: E.Exp -> T.Type -> FreestState T.TypeMap
outChoiceMap = choiceMap T.Out "external"

inChoiceMap :: E.Exp -> T.Type -> FreestState T.TypeMap
inChoiceMap = choiceMap T.In "internal"

choiceMap :: T.Polarity -> String -> E.Exp -> T.Type -> FreestState T.TypeMap
choiceMap pol msg e t =
  case normalise t of
    (T.Choice _ pol' m) ->
      if pol == pol' then return m else choiceErr t
    (T.Semi _ (T.Choice _ pol' m) u) -> if pol == pol'
      then return $ Map.map (\v -> T.Semi (pos v) v u) m
      else choiceErr t
    u -> choiceErr u
 where
  choiceErr :: T.Type -> FreestState T.TypeMap
  choiceErr u = do
    addError
      (pos e)
      [ Error $ "Expecting an " ++ msg ++ " choice type for expression"
      , Error e
      , Error "\n\t found type"
      , Error u
      ]
    return Map.empty

datatypeMap :: E.Exp -> T.Type -> FreestState T.TypeMap
datatypeMap e t =
  case normalise t of
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

constructor :: Pos -> T.TypeMap -> ProgVar -> FreestState T.Type
constructor p tm x = case tm Map.!? x of
  Just t  -> return t
  Nothing -> do
    addError p [Error "Constructor", Error x, Error "not in scope"]
    return $ omission p
