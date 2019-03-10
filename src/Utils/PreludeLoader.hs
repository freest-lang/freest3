module Utils.PreludeLoader (prelude) where

import qualified Data.Map.Strict as Map
import           Parse.Parser
import           Syntax.Programs (VarEnv)
import           Syntax.Types

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


-- TODO: REF - ver pos
preludeLoad :: VarEnv -> VarEnv
preludeLoad map =
  foldl (\acc (tv, t) -> Map.insert tv ((0,0),(TypeScheme [] (read t :: Type))) acc) map typeList

schemeLoad :: VarEnv -> VarEnv
schemeLoad map =
  foldl (\acc (tv, t) -> Map.insert tv ((0,0),(read t :: TypeScheme)) acc) map schemeList

