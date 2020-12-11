module Parse.Read where

import           Parse.Lexer                    ( Token )
import           Parse.Parser
import           Syntax.Kind
import           Syntax.Type
import           Syntax.Expression
import           Parse.ParseUtils
import           Control.Monad.State
import           Utils.FreestState

instance Read Kind where
  readsPrec _ s = [(parseKind s, "")]

instance Read Type where
  readsPrec _ = parser types

instance Read Exp where
  readsPrec _ = parser expr

parser :: ([Token] -> FreestStateT a) -> String -> [(a, String)]
parser parseFun str =
  case runStateT (lexer str "" parseFun) (initialState "") of
    Ok (t, state) ->
      if hasErrors state then error $ getErrors state else [(t, "")]
    Failed err -> error err
