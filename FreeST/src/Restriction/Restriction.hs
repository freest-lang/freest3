{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Restriction
    ( Inequality
    , Leveled(..)
    -- , minLevel
    -- , maxLevel
    , equalLevels
    , checkLevelRange
    )
where

import           Syntax.Base
import qualified Syntax.Type as T

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace (trace)

type Inequality = (T.Level, T.Level)

class Leveled a where
    level :: a -> T.Level

instance Leveled T.Type where
    level (T.Int _) = T.Top
    level (T.Float _) = T.Top
    level (T.Char _) = T.Top
    level (T.String _) = T.Top
    level (T.Arrow _ _ l1 _ _ _) = l1
    level (T.Labelled _ (T.Choice _) l _) = l
    level (T.Labelled _ T.Record _ m) = T.Top --level m --this rule is never supposed to be used
    level (T.Labelled _ T.Variant _ m) = T.Top --level m --this rule is never supposed to be used
    level (T.Skip _) = T.Top
    level (T.End _ _ l) = l
    level (T.Semi _ t1 t2) = level t1
    level (T.Message _ l _ _) = l
    level (T.Forall _ _) = T.Top 
    level (T.Rec _ _) = T.Top
    level (T.Var _ _) = T.Top
    level (T.Dualof _ t) = level t
    level (T.PForall _ _) = T.Top

-- instance Leveled T.TypeMap where
--     level tm
--         | Map.null tm = T.Top
--         | otherwise = trace ("  NOT " ++ show tm) $ do
--             foldr minLevel T.Top (map level (Map.elems tm))

-- instance Leveled T.TypeMap where
--     level tm
--         | Map.null tm = T.Top
--         | otherwise = foldr minLevel T.Top (map level (Map.elems tm))
    --   where
    --     e (T.Labelled _ T.Record _ m)  = levelOfTypeMap m
    --     e (T.Labelled _ T.Variant _ m) = levelOfTypeMap m
    --     e t                           = level t

-- instance Leveled T.TypeMap where
--     level tm
--         | Map.null tm = T.Top
--         | otherwise = minLevel' (getSpan tm) (map level (Map.elems tm))

equalLevels :: T.Type -> T.Type -> Bool
equalLevels (T.Arrow _ _ l1 l2 t1 t2) (T.Arrow _ _ l3 l4 u1 u2) =
    -- l1 == l3 && l2 == l4 && equalLevels t1 u1 && equalLevels t2 u2
    l1 == l3 && checkDefaultAbs l1 l2 l4 && equalLevels t1 u1 && equalLevels t2 u2
equalLevels (T.Labelled _ (T.Choice _) l1 m1) (T.Labelled _ (T.Choice _) l2 m2) =
    l1 == l2 && isTypeMapLevelEqual m1 m2
equalLevels (T.End _ _ l1) (T.End _ _ l2) =
    l1 == l2
equalLevels (T.Semi _ t1 t2) (T.Semi _ u1 u2) =
    equalLevels t1 u1 && equalLevels t2 u2
equalLevels (T.Message _ l1 _ _) (T.Message _ l2 _ _) =
    l1 == l2
equalLevels t1 t2 = True

checkDefaultAbs :: T.Level -> T.Level -> T.Level -> Bool
checkDefaultAbs T.Top T.Bottom _ = True
checkDefaultAbs _ l1 l2 = l1 == l2

isTypeMapLevelEqual :: (Ord k, Eq k) => Map.Map k T.Type -> Map.Map k T.Type -> Bool
isTypeMapLevelEqual m1 m2 =
    Map.keysSet m1 == Map.keysSet m2 &&
    and [equalLevels t1 t2 | (k, t1) <- Map.toList m1, let t2 = m2 Map.! k]

checkLevelRange :: T.Level -> T.LevelRange -> Bool
checkLevelRange l (l1, l2) = levelGT l l1 && levelLT l l2 

--read as l1 > l2
levelGT :: T.Level -> T.Level -> Bool
levelGT l1 l2 =
  case (evalLevel l1, evalLevel l2) of
    (Just n1, Just n2) -> n1 > n2
    _ ->
      case (l1, l2) of
        (T.Top, _) -> True
        (_, T.Top) -> False
        (_, T.Bottom) -> True
        (T.Bottom, _) -> False
        (T.LParens x, y) -> levelGT x y
        (x, T.LParens y) -> levelGT x y
        _ -> False

--read as l1 < l2
levelLT :: T.Level -> T.Level -> Bool
levelLT l1 l2 =
  case (evalLevel l1, evalLevel l2) of
    (Just n1, Just n2) -> n1 < n2
    _ ->
      case (l1, l2) of
        (T.Bottom, _) -> True
        (_, T.Bottom) -> False
        (_, T.Top) -> True
        (T.Top, _) -> False
        (T.LParens x, y) -> levelLT x y
        (x, T.LParens y) -> levelLT x y
        _ -> False

evalLevel :: T.Level -> Maybe Int
evalLevel (T.LNum n) = Just n
evalLevel (T.LAdd l1 l2) = (+) <$> evalLevel l1 <*> evalLevel l2
evalLevel (T.LParens l) = evalLevel l
evalLevel _ = Nothing


-- minLevel :: T.Level -> T.Level -> T.Level
-- minLevel T.Bottom _ = T.Bottom
-- minLevel _ T.Bottom = T.Bottom
-- minLevel T.Top l = l
-- minLevel l T.Top = l
-- minLevel (T.Num n1) (T.Num n2) = T.Num (min n1 n2)

-- maxLevel :: T.Level -> T.Level -> T.Level
-- maxLevel T.Top _ = T.Top
-- maxLevel _ T.Top = T.Top
-- maxLevel T.Bottom l = l
-- maxLevel l T.Bottom = l
-- maxLevel (T.Num n1) (T.Num n2) = T.Num (max n1 n2)

-- levelOfTypeMap :: T.TypeMap -> T.Level
-- levelOfTypeMap tm
--     | Map.null tm = T.Top 
--     | otherwise = trace ("HERE: " ++ show tm) $ level tm