{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Validation.Primitives (binops, isPrimitive) where

import           Syntax.Base
import           Syntax.MkName
-- import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
-- import           Syntax.Program
import qualified Syntax.Type as T
-- import Parse.Read
import qualified Util.FreestState as ST (listType)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map

isPrimitive :: String -> Bool
isPrimitive = flip elem
  ["(+)", "(==)","(/=)","(<)","(>)","(<=)","(>=)","(++)"]

binops :: String -> NonEmpty T.Type
binops = \case
  "(+)"  -> intFloatTypes
  "(==)" -> allBasicTypes
  "(/=)" -> allBasicTypes
  "(<)"  -> allBasicTypes
  "(>)"  -> allBasicTypes
  "(<=)" -> allBasicTypes
  "(>=)" -> allBasicTypes
  "(++)" -> unarrow list (unarrow list list) -- [Int] -> [Int] -> [Int]
       :| [ unarrow str (unarrow str str) -- String -> String -> String
          ]

-- (^^) : String, List ?


-- Int, Float
-- (+.) : Float -> Float -> Float
-- (-.) : Float -> Float -> Float
-- (*.) : Float -> Float -> Float
-- (/.) : Float -> Float -> Float
-- sqrt : Float -> Float
-- absF : Float -> Float
-- negateF : Float -> Float
-- maxF : Float -> Float -> Float
-- minF : Float -> Float -> Float

-- truncate : Float -> Int
-- round : Float -> Int
-- ceiling : Float -> Int
-- floor : Float -> Int
-- exp : Float -> Float
-- log : Float -> Float
-- sqrt : Float -> Float
-- logBase : Float -> Float -> Float -- x4


-- recip : Float -> Float
-- (**) : Float -> Float -> Float

-- sin : Float -> Float
-- cos : Float -> Float
-- tan : Float -> Float
-- asin: Float -> Float
-- acos: Float -> Float
-- atan: Float -> Float
-- sinh: Float -> Float
-- cosh: Float -> Float
-- tanh: Float -> Float
-- log1p: Float -> Float
-- expm1: Float -> Float
-- log1pexp: Float -> Float
-- log1mexp: Float -> Float
-- fromInteger: Int -> Float


-- power ^

-- negateF,absF, minF, maxF  : Int, Float


intFloatTypes :: NonEmpty T.Type
intFloatTypes = unarrow int (unarrow int int)
  :| [ unarrow float (unarrow float float) ]

-- TODO: Float (later)
allBasicTypes :: NonEmpty T.Type
allBasicTypes = unarrow int (unarrow int bool) -- Int -> Int -> Bool
  :| [ unarrow bool (unarrow bool bool) -- Bool -> Bool -> Bool 
     , unarrow char (unarrow char bool) -- Char -> Char -> Bool
     , unarrow str (unarrow str bool) -- String -> String -> Bool
     , unarrow unit (unarrow unit bool) -- () -> () -> Bool
     , unarrow float (unarrow float bool) -- Float -> Float -> Bool
     ]

s = defaultSpan
unarrow = T.Arrow s Un
unit = T.unit s
int = T.Int s
char = T.Char s
str = T.String s
float = T.Float s
bool = -- T.Var s $ mkVar s "Bool"
  T.Labelled s T.Variant $ Map.fromList [(true, emptyRcd), (false, emptyRcd)]

false = mkVar s "False"
true = mkVar s "True"
emptyRcd = T.Labelled s T.Record Map.empty

list = -- recListType
  T.Rec s (Bind s (mkList s) (K.ut s) ST.listType)
