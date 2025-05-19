module Restriction.Ordering where
--     ( 
--     , minLevel'
--     , maxLevel'
--     ) where

-- import           Syntax.Base
-- import qualified Syntax.Type as T
-- import           Parse.Unparser
-- import           Util.State (getLevelVarCounter, incrementLevelVarCounter, addInequality)

-- -- import qualified Data.Map.Strict as Map
-- -- import qualified Data.Set as Set

-- minLevel' ::  Span -> [T.Level] -> m T.Level
-- minLevel' span ls = do
--   n <- getLevelVarCounter
--   let newLevel = T.Num n
--   incrementLevelVarCounter
--   mapM_ (\l -> addInequality span (newLevel, l)) ls
--   return newLevel

-- maxLevel' ::  Span -> [T.Level] -> m T.Level
-- maxLevel' span ls = do
--   n <- getLevelVarCounter
--   let newLevel = T.Num n
--   incrementLevelVarCounter
--   mapM_ (\l -> addInequality span (l, newLevel)) ls
--   return newLevel