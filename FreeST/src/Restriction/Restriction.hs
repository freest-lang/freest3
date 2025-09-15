{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Restriction
    ( Inequality
    , Equality
    , InequalityEntry(..)
    , EqualityEntry(..)
    , Leveled(..)
    -- , minLevel
    -- , maxLevel
    , equalLevels
    -- , checkLevelRange
    )
where

import           Syntax.Base
import qualified Syntax.Type as T

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace (trace)

type Inequality = (T.Level, T.Level)
type Equality = (T.Level, T.Level)

data InequalityEntry = InequalityEntry
  { iSpan           :: Span
  , inequality      :: Inequality
  , iFunction       :: String
  , iThreadNum      :: Int
  } deriving (Eq, Ord)

data EqualityEntry = EqualityEntry
  { eSpan           :: Span
  , equality        :: Equality
  , eFunction       :: String
  , eThreadNum      :: Int
  } deriving (Eq, Ord)

class Leveled a where
    level :: a -> T.Level

instance Leveled T.Type where
    level (T.Int _) = T.Top
    level (T.Float _) = T.Top
    level (T.Char _) = T.Top
    level (T.String _) = T.Top
    level (T.Arrow _ _ l1 _ _ _) = l1
    level (T.Labelled _ (T.Choice _) l _) = l
    level (T.Labelled _ T.Record _ m) = T.Top
    level (T.Labelled _ T.Variant _ m) = T.Top
    level (T.Skip _) = T.Top
    level (T.End _ _ l) = l
    level (T.Semi _ t1 t2) = level t1
    level (T.Message _ l _ _) = l
    level (T.Forall _ _) = T.Top 
    level (T.Rec _ (Bind _ _ _ t)) = level t
    level (T.Var _ _) = T.Top
    level (T.Dualof _ t) = level t
    level (T.PForall _ (Bind _ _ _ t)) = level t

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
equalLevels (T.Arrow _ _ l1 l2 t1 t2) (T.Arrow _ _ l3 l4 u1 u2) = do
    compareLevels l1 l3 && checkDefaultAbs l1 l2 l4 && equalLevels t1 u1 && equalLevels t2 u2
    -- checkDefaultAbs l1 l2 l4 && equalLevels t1 u1 && equalLevels t2 u2
equalLevels (T.Labelled _ (T.Choice _) l1 m1) (T.Labelled _ (T.Choice _) l2 m2) =
    compareLevels l1 l2 && isTypeMapLevelEqual m1 m2
equalLevels (T.End _ _ l1) (T.End _ _ l2) =
    compareLevels l1 l2
equalLevels (T.Semi _ t1 t2) (T.Semi _ u1 u2) =
    equalLevels t1 u1 && equalLevels t2 u2
equalLevels (T.Message _ l1 _ _) (T.Message _ l2 _ _) =
    compareLevels l1 l2
equalLevels (T.PForall _ (Bind _ _ (r1,r2) t)) (T.PForall _ (Bind _ _ (r3,r4) u)) = do
    compareLevels r1 r3 && compareLevels r2 r4 && equalLevels t u
equalLevels t1 t2 = True

checkDefaultAbs :: T.Level -> T.Level -> T.Level -> Bool
checkDefaultAbs T.Top T.Bottom _ = True
checkDefaultAbs _ l1 l2 = compareLevels l1 l2

isTypeMapLevelEqual :: (Ord k, Eq k) => Map.Map k T.Type -> Map.Map k T.Type -> Bool
isTypeMapLevelEqual m1 m2 =
    Map.keysSet m1 == Map.keysSet m2 &&
    and [equalLevels t1 t2 | (k, t1) <- Map.toList m1, let t2 = m2 Map.! k]

compareLevels :: T.Level -> T.Level -> Bool
compareLevels T.Top T.Top = True
compareLevels T.Bottom T.Bottom = True
compareLevels (T.LVar x) (T.LVar y) = extern x == extern y
compareLevels (T.LNum n1) (T.LNum n2) = n1 == n2
compareLevels (T.LAdd l1 l2) (T.LAdd l3 l4) = compareLevels l1 l3 && compareLevels l2 l4
compareLevels (T.LParens l1) (T.LParens l2) = compareLevels l1 l2
compareLevels _ _ = False

toString :: T.Level -> String
toString T.Top = "top"
toString T.Bottom = "bot"
toString (T.LVar x) = extern x
toString (T.LNum n) = show n
toString (T.LAdd l1 l2) = toString l1 ++ "+" ++ toString l2
toString (T.LParens l) = "(" ++ toString l ++ ")"

-- checkLevelRange :: T.Level -> T.LevelRange -> Bool
-- checkLevelRange l (l1, l2) = levelGT l l1 && levelLT l l2 

-- --read as l1 > l2
-- levelGT :: T.Level -> T.Level -> Bool
-- levelGT l1 l2 =
--   case (evalLevel l1, evalLevel l2) of
--     (Just n1, Just n2) -> n1 > n2
--     _ ->
--       case (l1, l2) of
--         (T.Top, _) -> True
--         (_, T.Top) -> False
--         (_, T.Bottom) -> True
--         (T.Bottom, _) -> False
--         (T.LParens x, y) -> levelGT x y
--         (x, T.LParens y) -> levelGT x y
--         _ -> False

-- --read as l1 < l2
-- levelLT :: T.Level -> T.Level -> Bool
-- levelLT l1 l2 =
--   case (evalLevel l1, evalLevel l2) of
--     (Just n1, Just n2) -> n1 < n2
--     _ ->
--       case (l1, l2) of
--         (T.Bottom, _) -> True
--         (_, T.Bottom) -> False
--         (_, T.Top) -> True
--         (T.Top, _) -> False
--         (T.LParens x, y) -> levelLT x y
--         (x, T.LParens y) -> levelLT x y
--         _ -> False

-- evalLevel :: T.Level -> Maybe Int
-- evalLevel (T.LNum n) = Just n
-- evalLevel (T.LAdd l1 l2) = (+) <$> evalLevel l1 <*> evalLevel l2
-- evalLevel (T.LParens l) = evalLevel l
-- evalLevel _ = Nothing


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