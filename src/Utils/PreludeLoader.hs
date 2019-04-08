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
           , ("id", "forall a : SU => a -> a")
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
