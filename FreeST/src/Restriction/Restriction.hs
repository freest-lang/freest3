{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Restriction
    ( Inequality
    , Leveled(..)
    , minLevel
    , maxLevel
    )
where

import           Syntax.Base
import qualified Syntax.Type as T
import qualified Syntax.Kind as K

import qualified Data.Map.Strict as Map

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
    level (T.Labelled _ T.Record _ m) = level m
    level (T.Labelled _ T.Variant _ m) = level m
    level (T.Skip _) = T.Top
    level (T.End _ _ l) = l
    level (T.Semi _ t1 t2) = level t1
    level (T.Message _ l _ _) = l
    level (T.Forall _ _) = T.Top 
    level (T.Rec _ _) = T.Top
    level (T.Var _ _) = T.Top --T.Bottom --check these 3
    level (T.Dualof _ t) = level t

instance Leveled T.TypeMap where
    level tm
        | Map.null tm = T.Top
        | otherwise = foldr minLevel T.Top (map level (Map.elems tm))

minLevel :: T.Level -> T.Level -> T.Level
minLevel T.Bottom _ = T.Bottom
minLevel _ T.Bottom = T.Bottom
minLevel T.Top l = l
minLevel l T.Top = l
minLevel (T.Num n1) (T.Num n2) = T.Num (min n1 n2)

maxLevel :: T.Level -> T.Level -> T.Level
maxLevel T.Top _ = T.Top
maxLevel _ T.Top = T.Top
maxLevel T.Bottom l = l
maxLevel l T.Bottom = l
maxLevel (T.Num n1) (T.Num n2) = T.Num (max n1 n2)