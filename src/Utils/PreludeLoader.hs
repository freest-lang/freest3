module Utils.PreludeLoader (prelude) where

import           Syntax.Programs (VarEnv)
import           Syntax.Types
import           Syntax.Position
import           Parse.Parser
import qualified Data.Map.Strict as Map

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

-- TODO: add more           
schemeList :: [(String, String)]
schemeList = [
-- ("fst", "forall a, b => (a, b) -> a") -- fst/snd applies only to un-pairs but our pairs are lin
  ]
     
prelude :: VarEnv
prelude =
  schemeLoad (preludeLoad Map.empty)


-- TODO: what position fits here
preludeLoad :: VarEnv -> VarEnv
preludeLoad map =
  foldl (\acc (tv, t) ->
     Map.insert (Bind (AlexPn 0 0 0) tv)
                (TypeScheme (AlexPn 0 0 0) [] (read t :: Type)) acc) map typeList

schemeLoad :: VarEnv -> VarEnv
schemeLoad map =
  foldl (\acc (tv, t) -> Map.insert (Bind (AlexPn 0 0 0) tv) (read t :: TypeScheme) acc) map schemeList

