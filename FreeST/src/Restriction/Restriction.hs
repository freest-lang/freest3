{-# LANGUAGE FlexibleInstances #-}

module Restriction.Restriction
    ( Inequality
    , Equality
    , InequalityEntry(..)
    , EqualityEntry(..)
    , Leveled(..)

    , equalLevels
    , compareLevels
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
compareLevels _ _ = False
