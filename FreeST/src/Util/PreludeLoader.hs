{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  -- , isBuiltin
  , userDefined
  -- , preludeNamingCtx
  )
where

import           Syntax.Base
import qualified Syntax.Type                   as T
import           Syntax.Program
import qualified Data.Map.Strict               as Map
import           Parse.Read                     ( )

import           Parse.Parser
import Util.FreestState

typeList :: [(Variable, T.Type)]
typeList =
  [ -- Int
    (mkVar p "(+)"     , readT "Int -> Int -> Int")
  , (mkVar p "(-)"     , readT "Int -> Int -> Int")
  , (mkVar p "(*)"     , readT "Int -> Int -> Int")
  , (mkVar p "(/)"     , readT "Int -> Int -> Int")
  , (mkVar p "(^)"     , readT "Int -> Int -> Int")
  , (mkVar p "mod"     , readT "Int -> Int -> Int")
  , (mkVar p "rem"     , readT "Int -> Int -> Int")
  , (mkVar p "div"     , readT "Int -> Int -> Int")
  , (mkVar p "max"     , readT "Int -> Int -> Int")
  , (mkVar p "min"     , readT "Int -> Int -> Int")
  , (mkVar p "quot"    , readT "Int -> Int -> Int")
  , (mkVar p "gcd"     , readT "Int -> Int -> Int")
  , (mkVar p "lcm"     , readT "Int -> Int -> Int")
  , (mkVar p "subtract", readT "Int -> Int -> Int")
  , (mkVar p "succ"    , readT "Int -> Int")
  , (mkVar p "pred"    , readT "Int -> Int")
  , (mkVar p "abs"     , readT "Int -> Int")
  , (mkVar p "negate"  , readT "Int -> Int")
  , (mkVar p "even"    , readT "Int -> Bool")
  , (mkVar p "odd"     , readT "Int -> Bool")
  , (mkVar p "(==)"    , readT "Int -> Int -> Bool")
  , (mkVar p "(/=)"    , readT "Int -> Int -> Bool")
  , (mkVar p "(<)"     , readT "Int -> Int -> Bool")
  , (mkVar p "(>)"     , readT "Int -> Int -> Bool")
  , (mkVar p "(<=)"    , readT "Int -> Int -> Bool")
  , (mkVar p "(>=)"    , readT "Int -> Int -> Bool")
  -- Bool
  , (mkVar p "not" , readT "Bool -> Bool")
  , (mkVar p "(&&)", readT "Bool -> Bool -> Bool")
  , (mkVar p "(||)", readT "Bool -> Bool -> Bool")
  -- Char
  , (mkVar p "ord", readT "Char -> Int")
  , (mkVar p "chr", readT "Int -> Char")
  -- Pair
  , (mkVar p "fst", readT "∀ a:1T . ∀ b:*T . (a, b) -> a")
  , (mkVar p "snd", readT "∀ a:*T . ∀ b:1T . (a, b) -> b")
  -- Print
  , (mkVar p "printInt"   , readT "Int -> ()")
  , (mkVar p "printIntLn" , readT "Int -> ()")
  , (mkVar p "printBool"  , readT "Bool -> ()")
  , (mkVar p "printBoolLn", readT "Bool -> ()")
  , (mkVar p "printChar"  , readT "Char -> ()")
  , (mkVar p "printCharLn", readT "Char -> ()")
  , (mkVar p "printUnit"  , readT "() -> ()")
  , (mkVar p "printUnitLn", readT "() -> ()")
  , (mkVar p "printString", readT "String -> ()")
  , (mkVar p "printStringLn", readT "String -> ()")
  -- Fork
  , (mkVar p "fork", readT "∀a:1T. a -> ()")
  -- Error & Undefined
  , (mkVar p "error", readT "∀a:*T . String -> a")
  , (mkVar p "undefined", readT "∀a:*T . a")
  -- Session ops
  , (mkVar p "send", readT "∀a:1T . a -> ∀b:1S . !a;b 1-> b")
  , (mkVar p "receive", readT "∀a:1T . ∀b:1S . ?a;b -> (a, b)")
  -- Not the actual type for collect, but for writing it we would
  -- need polymorphism over the labels in some choice/variant
  , (mkVar p "collect", read "∀a:1T . a") 

  ]
  where p = defaultSpan {defModule = "Prelude"}


-- Similar to the Read instance, but it enforces the type's module to be Prelude.
class ReadType t where
  readT :: String -> t

instance ReadType T.Type where
  readT s = either (error . getErrors . (\errors -> initialState {errors})) id (parseType "Prelude" s)

prelude :: VarEnv
prelude = Map.fromList typeList-- foldr (uncurry Map.insert) Map.empty typeList

isBuiltin :: Variable -> Bool
isBuiltin = (`elem` map fst typeList)

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))

-- Names from the prelude, in order 
preludeNamingCtx :: [String]
preludeNamingCtx = map intern $ Map.keys prelude
