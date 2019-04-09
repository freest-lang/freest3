module Utils.PreludeLoader
( prelude
, isBuiltin
) where

import           Parse.Lexer (defaultPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Bind
import           Parse.Parser
import qualified Data.Map.Strict as Map

typeList :: [(PVar, String)]
typeList = [ (PVar "(+)", "Int -> Int -> Int")
           , (PVar "(-)", "Int -> Int -> Int")
           , (PVar "(/)", "Int -> Int -> Int")
           , (PVar "(*)", "Int -> Int -> Int")
           , (PVar "mod", "Int -> Int -> Int")
           , (PVar "rem", "Int -> Int -> Int")
           , (PVar "div", "Int -> Int -> Int")
           , (PVar "negate", "Int -> Int")
           , (PVar "not", "Bool -> Bool")
           , (PVar "(&&)", "Bool -> Bool -> Bool")
           , (PVar "(||)", "Bool -> Bool -> Bool")
           , (PVar "(==)", "Int -> Int -> Bool")
           , (PVar "(<)", "Int -> Int -> Bool")
           , (PVar "(>)", "Int -> Int -> Bool")
           , (PVar "(<=)", "Int -> Int -> Bool")
           , (PVar "(>=)", "Int -> Int -> Bool")
           , (PVar "id", "forall a : SU => a -> a")
           ]

prelude :: VarEnv
prelude =
  foldl (\acc (v, t) ->
     Map.insert (PBind defaultPos v) (read t) acc) Map.empty typeList

isBuiltin :: PBind -> Bool
isBuiltin (PBind _ x) = x `elem` (map fst typeList)

{-
preludeLoad :: VarEnv -> VarEnv
preludeLoad map =
  foldl (\acc (tv, t) ->
     Map.insert (PBind defaultPos tv)
                (read t :: TypeScheme) acc) map typeList

schemeList :: [(String, String)]
schemeList = [
--  ("id", "forall a : SU => a -> a")
  -- ("fst", "forall a, b => (a, b) -> a") -- fst/snd applies only to un-pairs but our pairs are lin
  ]
schemeLoad :: VarEnv -> VarEnv
schemeLoad map =
  foldl (\acc (tv, t) -> Map.insert (PBind defaultPos tv) (read t :: TypeScheme) acc) map schemeList
-}     
