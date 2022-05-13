module Elaboration.Match
  ( match
  )
where

import           Syntax.Expression as E
import qualified Data.Map.Strict   as Map

-- type FieldMap  = Map.Map Variable ([Variable], Exp)
-- type FieldMapP = Map.Map Variable ([Pattern], Exp)

-- TODO
match :: E.FieldMapP -> E.FieldMap
match fm = undefined

ruleEmpty :: Int -> Int
ruleEmpty x = x

ruleVar :: Int -> Int
ruleVar x = x

ruleCon :: Int -> Int
ruleCon x = x

ruleMix :: Int -> Int
ruleMix x = x