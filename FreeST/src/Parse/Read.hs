{-# LANGUAGE NamedFieldPuns,TupleSections #-}
module Parse.Read where

import           Parse.Parser
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Type
import           Util.FreestState

instance Read Kind where
  readsPrec _ = eitherRead parseKind

instance Read Type where
  readsPrec _ = eitherRead parseType

instance Read Exp where
  readsPrec _ = eitherRead parseExpr

eitherRead :: (String -> Either Errors a) -> String -> [(a, String)]
eitherRead f s =
  either (error . getErrors [] . state) ((:[]) . (,"")) (f s) 
  where
    state errors = initialState {errors}
