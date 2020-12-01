module Parse.Read where

import           Parse.Lexer                    ( Token )
import           Parse.Parser
import           Syntax.Kind
import           Syntax.Types
import           Syntax.Expression
import           Parse.ParseUtils
import           Syntax.Base
import           Control.Monad.State
import           Utils.FreestState
import           Data.Char                      ( isSpace )

instance Read Kind where
  readsPrec _ s = [(parseKind s, "")]

instance Read Type where
  readsPrec _ = parser types

instance Read Exp where
  readsPrec _ = parser expr

parser :: ([Token] -> FreestStateT a) -> String -> [(a, String)]
parser parseFun str =
  case runStateT (parse str "" parseFun) (initialState "") of
    Ok (t, state) ->
      if hasErrors state then error $ getErrors state else [(t, "")]
    Failed err -> error err
