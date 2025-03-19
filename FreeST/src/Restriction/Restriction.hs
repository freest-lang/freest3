module Restriction.Restriction
    ( Inequality
    , Leveled(..)
    )
where

import           Syntax.Base
import qualified Syntax.Type as T
-- import           Syntax.Kind

type Inequality = (T.Level, T.Level)

class Leveled a where
    level :: a -> T.Level

instance Leveled T.Type where
    level (T.Int _) = T.Bottom
    level (T.Float _) = T.Bottom
    level (T.Char _) = T.Bottom
    level (T.String _) = T.Bottom
    -- level (T.Arrow _ _ t1 t2) =
    -- level (T.Labelled _ l _ _) = l
    level (T.Skip _) = T.Top
    -- level (T.End _ l _) = l
    level (T.Semi _ t1 t2) = level t1
    level (T.Message _ l _ _) = l
    -- level (T.Forall _ (T.Bind _ k t)) = 
    -- level (T.Rec _ (T.Bind _ k t)) = not supported yet
    level (T.Var _ _) = T.Bottom
    level (T.Dualof _ t) = level t