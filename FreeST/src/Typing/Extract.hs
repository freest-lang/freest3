{-|
Module      :  Typing.Extract
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

module Typing.Extract
  ( function
  , pair
  , forall
  , output
  , input
  , outChoiceMap
  , inChoiceMap
  , datatypeMap
  , choiceBranch -- for select C e
  )
where

import           Syntax.Base
import qualified Syntax.Expression    as E
import qualified Syntax.Type          as T
import           Syntax.MkName        (mkTupleLabels)
import           Typing.Normalisation ( normalise )
import           Typing.Phase
import           Util.Error
import           Util.State

import           Data.Functor
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set


function :: E.Exp -> T.Type -> TypingState (T.Type, T.Type)
function e t =
  case normalise t of
    (T.Arrow _ _ u v) -> return (u, v)
    u -> let p = getSpan e in
      addError (ExtractError p "an arrow" e u) $> (omission p, omission p)

pair :: E.Exp -> T.Type -> TypingState (T.Type, T.Type)
pair e t =
  case normalise t of
    (T.Labelled _ T.Record m) | Map.keysSet m == Set.fromList [l0, l1] ->
      return (m Map.! l0, m Map.! l1)
    u -> let p = getSpan u in
      addError (ExtractError p "a pair" e u) $> (omission p, omission p)
  where l0 = head mkTupleLabels defaultSpan
        l1 = (mkTupleLabels !! 1) defaultSpan 

forall :: E.Exp -> T.Type -> TypingState T.Type
forall e t =
  case normalise t of
    u@T.Forall{} -> return u
    u -> let p = getSpan e in
      addError (ExtractError p "a polymorphic" e u) $> T.Forall p (omission p)

output :: E.Exp -> T.Type -> TypingState (T.Type, T.Type)
output = message T.Out "an output"

input :: E.Exp -> T.Type -> TypingState (T.Type, T.Type)
input = message T.In "an input"

message :: T.Polarity -> String -> E.Exp -> T.Type -> TypingState (T.Type, T.Type)
message pol msg e t =
  case normalise t of
    T.Semi _ (T.Message _ pol' u) v | pol == pol' -> return (u, v)
    u -> addError (ExtractError (getSpan e) msg e u) $>
           (omission $ getSpan u, T.Skip $ getSpan u)

outChoiceMap :: E.Exp -> T.Type -> TypingState T.TypeMap
outChoiceMap = choiceMap T.External "an external choice (&)"

inChoiceMap :: E.Exp -> T.Type -> TypingState T.TypeMap
inChoiceMap = choiceMap T.Internal "an internal choice (+)"

choiceMap :: T.View -> String -> E.Exp -> T.Type -> TypingState T.TypeMap
choiceMap view msg e t =
  case normalise t of
    (T.Semi _ (T.Labelled _ (T.Choice view') m) u) | view == view' ->
      return $ Map.map (\v -> T.Semi (getSpan v) v u) m
    u -> addError (ExtractError (getSpan e) msg e u) $> Map.empty

datatypeMap :: E.Exp -> T.Type -> TypingState T.TypeMap
datatypeMap e t =
  case normalise t of
    (T.Labelled _ T.Variant m) -> return m
    u -> addError (ExtractError (getSpan e) "a datatype" e u) $> Map.empty

choiceBranch :: Span -> T.TypeMap -> Variable -> T.Type -> TypingState T.Type
choiceBranch p tm a t = case tm Map.!? a of
  Just t -> return t
  Nothing -> addError (BranchNotInScope p a t) $> omission p

