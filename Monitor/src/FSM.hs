module FSM (check, TypeOfMessage(..)) where
import Parse.Parser (parseType)
import qualified Syntax.Type as T
import qualified Syntax.Base as B

import Parse.Lexer (scanTokens)
import Typing.Normalisation (normalise)
import qualified Data.Map.Strict               as Map
import Data.Maybe (isNothing, isJust)
import           Debug.Trace (trace)

import Equivalence.AlphaCongruence

data TypeOfMessage  = Normal T.Type T.Polarity
                    | Finish T.Type
                    | Label  B.Variable T.View

check :: TypeOfMessage -> T.Type -> Either String T.Type

check (Normal t T.Out) sm = case output sm of
    Right (u, rest) -> if u == t then Right rest else Left $ "Expecting " ++ show u ++ " but got " ++ show t
    Left x -> Left x

check (Normal t T.In) sm = case input sm of
    Right (u, rest) -> if u == t then Right rest else Left $ "Expecting " ++ show u ++ " but got " ++ show t
    Left x -> Left x

check (Finish (T.End _ p)) sm = end sm p

check (Label label T.Internal) sm = case inChoiceMap sm of
    Right m ->
      case label `Map.lookup` m of
        Just a ->
          Right a
        Nothing ->
          Left $ "Label not found " ++ show label ++ " in " ++ show m
    Left x -> Left x

check (Label label T.External) sm = case outChoiceMap sm of
    Right m ->
      case label `Map.lookup` m of
        Just a ->
          Right a
        Nothing ->
          Left $ "Label not found " ++ show label  ++ " in " ++ show m
    Left x -> Left x

check _ b = Left $ "Not implemented " ++ show b

end :: T.Type -> T.Polarity -> Either String T.Type
end t p = case normalise t of
  T.End _ p' | p == p' -> Right $ T.Skip B.defaultSpan
  u -> Left $ "Expected an end type but got " ++ show u

output :: T.Type -> Either String (T.Type, T.Type)
output = message T.Out 

input :: T.Type -> Either String (T.Type, T.Type)
input = message T.In 

message :: T.Polarity -> T.Type -> Either String (T.Type, T.Type)
message pol t =
  case normalise t of
    T.Semi _ (T.Message _ pol' u) v | pol == pol' -> Right (u, v)
    u -> Left $ "Mistake on the direction of the message " ++ show pol ++ show u

outChoiceMap :: T.Type -> Either String T.TypeMap
outChoiceMap = choiceMap T.External 

inChoiceMap :: T.Type -> Either String T.TypeMap
inChoiceMap = choiceMap T.Internal

choiceMap :: T.View -> T.Type -> Either String T.TypeMap
choiceMap view t =
  case normalise t of
    (T.Semi _ (T.Labelled _ (T.Choice view') m) u) ->
      if view /= view' then Left $ "View mismatch " ++ show view ++ " " ++ show view' else
      Right $ Map.map (\v -> T.Semi B.defaultSpan v u) m
    u -> Left $ "Expected a choice type but got " ++ show u

datatypeMap :: T.Type -> Either String T.TypeMap
datatypeMap t =
  case normalise t of
    (T.Labelled _ T.Variant m) -> return m
    u -> Left $ "Expected a datatype but got " ++ show u

choiceBranch :: T.TypeMap -> B.Variable -> T.Type -> Either String T.Type
choiceBranch tm a t = case tm Map.!? a of
  Just t -> Right t
  Nothing -> Left $ "Branch not in scope " ++ show a ++ " " ++ show t


