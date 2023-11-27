{-# LANGUAGE NamedFieldPuns,TupleSections #-}
module Parse.Read where

import           Parse.Parser
import           Parse.Phase
import           Syntax.AST
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.Type
import           Util.State

instance Read Kind where
  readsPrec _ = eitherRead parseKind

instance Read Type where
  readsPrec _ = eitherRead parseType

instance Read Exp where
  readsPrec _ = eitherRead parseExpr

eitherRead :: (FilePath -> String -> Either Errors a) -> String -> [(a, String)]
eitherRead parseFun input =
  either (error . getErrors defaultOpts . state) ((:[]) . (,"")) (parseFun "" input) 
  where
    state errors = initialS {errors}
