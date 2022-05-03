{-# LANGUAGE FlexibleInstances #-}

module Parse.Read where

import           Control.Monad.State
import qualified Data.Map as Map
import           Parse.Lexer ( Token )
import           Parse.ParseUtils
import           Parse.Parser
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Type
import           Util.Error
import           Util.FreestState

instance Read Kind where
  readsPrec _ s = [(parseKind s, "")]

instance Read Type where
  readsPrec _ = parser types

instance Read Exp where
  readsPrec _ = parser expr

parser :: ([Token] -> FreestStateT a) -> String -> [(a, String)]
parser parseFun str =
  case runStateT (lexer str "" parseFun) initialState of
    Ok (t, state) ->
      if hasErrors state
      then error $ getErrors [] state
      else [(t, "")]
    Failed err -> error $ formatError "" Map.empty [] err
