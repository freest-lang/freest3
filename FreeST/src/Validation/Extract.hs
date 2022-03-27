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

module Validation.Extract
  ( function
  , pair
  , forall
  , output
  , input
  , outChoiceMap
  , inChoiceMap
  , datatypeMap
  , choiceBranch
  )
where

import           Data.Functor
import qualified Data.Map.Strict as Map
import           Equivalence.Normalisation ( normalise )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState

function :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
function e t =
  case normalise t of
    (T.Arrow _ _ u v) -> return (u, v)
    u               -> let p = pos e in
      addError (ExtractError p "an arrow" e u) $> (omission p, omission p)

pair :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
pair e t =
  case normalise t of
    (T.Pair _ u v) -> return (u, v)
    u              -> let p = pos u in
      addError (ExtractError p "a pair" e u) $> (omission p, omission p)

forall :: E.Exp -> T.Type -> FreestState T.Type
forall e t =
  case normalise t of
    u@T.Forall{} -> return u
    u            -> let p = pos e in
      addError (ExtractError p "a polymorphic" e u) $> T.Forall p (omission p)

output :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
output = message T.Out "an output"

input :: E.Exp -> T.Type -> FreestState (T.Type, T.Type)
input = message T.In "an input"

message :: T.Polarity -> String -> E.Exp -> T.Type -> FreestState (T.Type, T.Type)
message pol msg e t =
  case normalise t of
    u@(T.Message p pol' b) ->
      if pol == pol' then return (b, T.Skip p) else messageErr u
    u@(T.Semi _ (T.Message _ pol' b) v) ->
      if pol == pol' then return (b, v) else messageErr u
    u -> messageErr u
 where
  messageErr :: T.Type -> FreestState (T.Type, T.Type)
  messageErr u = let p = pos e in
    addError (ExtractError p msg e u) $> (T.Unit $ pos u, T.Skip $ pos u)

outChoiceMap :: E.Exp -> T.Type -> FreestState T.TypeMap
outChoiceMap = choiceMap T.External "an external choice (&)"

inChoiceMap :: E.Exp -> T.Type -> FreestState T.TypeMap
inChoiceMap = choiceMap T.Internal "an internal choice (+)"

choiceMap :: T.View -> String -> E.Exp -> T.Type -> FreestState T.TypeMap
choiceMap view msg e t =
  case normalise t of
    (T.Choice _ view' m) ->
      if view == view' then return m else choiceErr t
    (T.Semi _ (T.Choice _ view' m) u) ->
      if view == view'
      then return $ Map.map (\v -> T.Semi (pos v) v u) m
      else choiceErr t
    u -> choiceErr u
 where
  choiceErr :: T.Type -> FreestState T.TypeMap
  choiceErr u = let p = pos e in
    addError (ExtractError p msg e u) $> Map.empty

datatypeMap :: E.Exp -> T.Type -> FreestState T.TypeMap
datatypeMap e t =
  case normalise t of
    (T.Variant _ m) -> return m
    u                -> let p = pos e in
      addError (ExtractError p "a datatype" e u) $> Map.empty

choiceBranch :: Pos -> T.TypeMap -> Variable -> T.Type -> FreestState T.Type
choiceBranch p tm x t = case tm Map.!? x of
  Just t  -> return t
  Nothing -> addError (BranchNotInScope p x t) $> omission p
