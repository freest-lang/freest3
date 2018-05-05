module PreludeLoader (prelude) where

import qualified Data.Map.Strict as Map
import           Terms.Terms
import           Types.TypeParser
import           Types.Types

typeList :: [(String, String)]
typeList = [ ("(+)", "Int -> Int -> Int")
           , ("(-)", "Int -> Int -> Int")
           , ("(/)", "Int -> Int -> Int")
           , ("(*)", "Int -> Int -> Int")
           , ("mod", "Int -> Int -> Int")
           , ("rem", "Int -> Int -> Int")
           , ("div", "Int -> Int -> Int")
           , ("negate", "Int -> Int")
           , ("not", "Bool -> Bool")
           , ("(&&)", "Bool -> Bool -> Bool")
           , ("(||)", "Bool -> Bool -> Bool")
           , ("(==)", "Int -> Int -> Bool")
           , ("(<)", "Int -> Int -> Bool")
           , ("(>)", "Int -> Int -> Bool")
           , ("(<=)", "Int -> Int -> Bool")
           , ("(>=)", "Int -> Int -> Bool")
           ]
           
prelude :: VarEnv
prelude = preludeLoad Map.empty

preludeLoad :: VarEnv -> VarEnv
preludeLoad map = foldl (\acc (tv, t) -> Map.insert tv (TypeScheme [] (read t :: Type)) acc) map typeList
