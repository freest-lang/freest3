module Monitor.FSM (check, TypeOfMessage(..)) where
import Parse.Parser (parseType)
import qualified Syntax.Type as T
import qualified Syntax.Base as B

import Parse.Lexer (scanTokens)
import Typing.Normalisation (normalise)
import Bisimulation.Norm (norm)
import qualified Data.Map.Strict               as Map
import Data.Maybe (isNothing, isJust)
import Equivalence.AlphaCongruence

-- Compare if two types are the same
-- compareTypes :: T.Type -> T.Type -> Maybe Variable -> Either String T.Type
-- compareTypes (T.Int _) (T.Int _) _       = Right $ T.Skip defaultSpan
-- compareTypes (T.Int _) t _               = Left ("Expected Int but got " ++ show t)

-- compareTypes (T.Float _) (T.Float _) _   = Right $ T.Skip defaultSpan
-- compareTypes (T.Float _) t _             = Left ("Expected Float but got " ++ show t)

-- compareTypes (T.Char _) (T.Char _) _     = Right $ T.Skip defaultSpan
-- compareTypes (T.Char _) t _              = Left ("Expected Char but got " ++ show t)

-- compareTypes (T.String _) (T.String _) _ = Right $ T.Skip defaultSpan
-- compareTypes (T.String _) t _ = Left ("Expected String but got " ++ show t)

-- compareTypes (T.End _ p) (T.End _ q)   _  = 
--     if p == q then
--         Right $ T.Skip defaultSpan 
--     else 
--         Left "It wasn't you to close the channel"

-- -- We receive from the layer above a message. First compare the polarity and then the type
-- compareTypes (T.Message _ p t) (T.Message _ q u) _ = 
--     if p /= q then 
--         Left "It wasn't your turn to send a message"
--     else
--         compareTypes t u Nothing

-- -- If is a Semi we first compare the first type and then return the rest. This is because we are going to wait
-- -- for the next message and compare that
-- compareTypes (T.Semi _ t u) z v = case compareTypes t z v of
--     Right _ -> Right u
--     Left x -> Left x


-- TODO()
-- If we receive a label we need to compare the direction then choose the path to follow
-- After that is using the rest

-- compareTypes (T.Labelled _ c m) (T.Labelled _ d n) (Just v) = 
--     (c == d && Map.keys m == Map.keys n && isJust a, case a of 
--         Just x -> x
--         Nothing -> T.Skip defaultSpan
--     )
--     where
--         a = Map.lookup v m

-- compareTypes _ _ _ = Left "Something went wrong"

data TypeOfMessage  = Normal T.Type T.Polarity
                    | Finish T.Type
                    | Label [B.Variable] B.Variable T.View


check :: TypeOfMessage -> T.Type -> Either String T.Type

check (Normal t T.Out) sm = case output sm of
    Right (u, rest) -> if u == t then Right rest else Left $ "Expecting " ++ show u ++ " but got " ++ show t
    Left x -> Left x

check (Normal t T.In) sm = case input sm of
    Right (u, rest) -> if u == t then Right rest else Left $ "Expecting " ++ show u ++ " but got " ++ show t
    Left x -> Left x

check (Finish (T.End _ p)) sm = end sm p

check (Label choiceId label T.Internal) sm = case inChoiceMap sm of
    Right m ->       
      let res = all (`Map.member` m) choiceId in 
      if res  && Map.size m == length choiceId then
        Right $ m Map.! label
      else
        Left "Label not found" 
    Left x -> Left x

check (Label choiceId label T.External) sm = case outChoiceMap sm of
    Right m ->       
      let res = all (`Map.member` m) choiceId in 
      if res  && Map.size m == length choiceId then
        Right $ m Map.! label
      else
        Left "Label not found" 
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
    u -> Left $ "Mistake on the direction of the message " ++ show pol

outChoiceMap :: T.Type -> Either String T.TypeMap
outChoiceMap = choiceMap T.External 

inChoiceMap :: T.Type -> Either String T.TypeMap
inChoiceMap = choiceMap T.Internal

choiceMap :: T.View -> T.Type -> Either String T.TypeMap
choiceMap view t =
  case normalise t of
    (T.Semi _ (T.Labelled _ (T.Choice view') m) u) | view == view' ->
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


