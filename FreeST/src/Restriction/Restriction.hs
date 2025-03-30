{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Restriction.Restriction
    ( Inequality
    , Leveled(..)
    , incrementLevel
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
    level (T.Int _) = T.Bottom
    level (T.Float _) = T.Bottom
    level (T.Char _) = T.Bottom
    level (T.String _) = T.Bottom
    level (T.Arrow _ _ l1 _ _ _) = l1
    level (T.Labelled _ (T.Choice _) l _) = l
    level (T.Labelled _ T.Record _ _) = T.Bottom --is this top or bot?
    level (T.Labelled _ T.Variant _ _) = T.Bottom --is this top or bot?
    level (T.Skip _) = T.Top
    level (T.End _ _ l) = l
    level (T.Semi _ t1 t2) = level t1
    level (T.Message _ l _ _) = l
    level (T.Forall _ _) = T.Bottom 
    level (T.Rec _ _) = T.Bottom
    level (T.Var _ _) = T.Bottom
    level (T.Dualof _ t) = level t

instance Leveled T.TypeMap where
    level tm
        | Map.null tm = T.Bottom
        | otherwise = foldr joinLevels T.Bottom (map level (Map.elems tm))
      where
        joinLevels :: T.Level -> T.Level -> T.Level
        joinLevels T.Top _ = T.Top
        joinLevels _ T.Top = T.Top
        joinLevels T.Bottom l = l
        joinLevels l T.Bottom = l
        joinLevels (T.Num n1) (T.Num n2) = T.Num (min n1 n2)

incrementLevel :: T.Level -> T.Level
incrementLevel (T.Num n) = T.Num (n + 1)
incrementLevel T.Top = T.Top -- ?
incrementLevel T.Bottom = T.Bottom