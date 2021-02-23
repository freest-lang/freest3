{- |
Module      :  Syntax.Types
Description :  Signatures for the functions in the prelude
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module introduces the signatures for the functions in the prelude
-}

module Util.PreludeLoader
  ( prelude
  , isBuiltin
  , userDefined
  )
where

import           Syntax.ProgramVariable
import           Syntax.Base
import qualified Syntax.Type                   as T
import           Syntax.Program
import qualified Data.Map.Strict               as Map
import           Parse.Read                     ( )

typeList :: [(ProgVar, T.Type)]
typeList =
  [ -- Integers
    (mkVar p "(+)"     , read "Int -> Int -> Int")
  , (mkVar p "(-)"     , read "Int -> Int -> Int")
  , (mkVar p "(/)"     , read "Int -> Int -> Int")
  , (mkVar p "(*)"     , read "Int -> Int -> Int")
  , (mkVar p "(^)"     , read "Int -> Int -> Int")
  , (mkVar p "mod"     , read "Int -> Int -> Int")
  , (mkVar p "rem"     , read "Int -> Int -> Int")
  , (mkVar p "div"     , read "Int -> Int -> Int")
  , (mkVar p "max"     , read "Int -> Int -> Int")
  , (mkVar p "min"     , read "Int -> Int -> Int")
  , (mkVar p "quot"    , read "Int -> Int -> Int")
  , (mkVar p "gcd"     , read "Int -> Int -> Int")
  , (mkVar p "lcm"     , read "Int -> Int -> Int")
  , (mkVar p "subtract", read "Int -> Int -> Int")
  , (mkVar p "succ"    , read "Int -> Int")
  , (mkVar p "pred"    , read "Int -> Int")
  , (mkVar p "abs"     , read "Int -> Int")
  , (mkVar p "negate"  , read "Int -> Int")
  , (mkVar p "even"    , read "Int -> Bool")
  , (mkVar p "odd"     , read "Int -> Bool")
  , (mkVar p "(==)"    , read "Int -> Int -> Bool")
  , (mkVar p "(/=)"    , read "Int -> Int -> Bool")
  , (mkVar p "(<)"     , read "Int -> Int -> Bool")
  , (mkVar p "(>)"     , read "Int -> Int -> Bool")
  , (mkVar p "(<=)"    , read "Int -> Int -> Bool")
  , ( mkVar p "(>=)"   , read "Int -> Int -> Bool")
  -- Bool
  , (mkVar p "not" , read "Bool -> Bool")
  , (mkVar p "(&&)", read "Bool -> Bool -> Bool")
  , ( mkVar p "(||)"
    , read "Bool -> Bool -> Bool"
    )
  -- Chars
  , (mkVar p "ord", read "Char -> Int")
  , ( mkVar p "chr", read "Int -> Char")
  -- Pairs
  , (mkVar p "fst", read "∀ a:TU . ∀ b:TU . (a, b) -> a")
  , ( mkVar p "snd", read "∀ a:TU . ∀ b:TU . (a, b) -> b")
  --  Prints
  , (mkVar p "printInt"   , read "Int -> ()")
  , (mkVar p "printIntLn" , read "Int -> ()")
  , (mkVar p "printBool"  , read "Bool -> ()")
  , (mkVar p "printBoolLn", read "Bool -> ()")
  , (mkVar p "printChar"  , read "Char -> ()")
  , (mkVar p "printCharLn", read "Char -> ()")
  , (mkVar p "printUnit"  , read "() -> ()")
  , (mkVar p "printUnitLn", read "() -> ()")
  , (mkVar p "printString", read "String -> ()")
  , ( mkVar p "printStringLn", read "String -> ()")
  -- Fork
  , ( mkVar p "fork", read "() -> ()")
  -- Error
  , (mkVar p "error", read "∀ a:TU . String -> a")
  ]
  where p = defaultPos

prelude :: VarEnv
prelude = Map.fromList typeList-- foldr (uncurry Map.insert) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` map fst typeList)

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))
