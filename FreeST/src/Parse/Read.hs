module Parse.Read where

import Parse.Lexer (Token)
import Parse.Parser
import Syntax.Kinds
import Syntax.Types
import Syntax.Expressions
import           Parse.ParseUtils
import Syntax.Base
import           Control.Monad.State
import           Utils.FreestState
import           Data.Char(isSpace)

instance Read Kind where
  readsPrec _ s = -- [(parseKind s, "")]
    tryParse [("SL", Kind defaultPos Session Lin),
              ("SU", Kind defaultPos Session Un),
              ("TL", Kind defaultPos Functional Lin),
              ("TU", Kind defaultPos Functional Un)]
    where tryParse [] = []
          tryParse ((attempt,result):xs) =
            if take (length attempt) (trim s) == attempt
            then [(result, drop (length attempt) (trim s))]
            else tryParse xs
          trim s = dropWhile isSpace s


instance Read Type where
  readsPrec _ = parser types

instance Read Expression where
  readsPrec _ = parser expr

parser :: ([Token] -> FreestStateT a) -> String -> [(a, String)]
parser parseFun str = 
   case runStateT (parse str "" parseFun) (initialState "") of
      Ok (t, state) ->
        if hasErrors state then error $ getErrors state else [(t, "")]
      Failed err -> error err
