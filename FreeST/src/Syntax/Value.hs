module Syntax.Value
  ( isVal
  )
where

import           Syntax.Base
import qualified Syntax.Expression             as E
import Syntax.MkName (mkSelect, mkSend, mkReceive, mkClose)

isVal :: E.Exp -> Bool
-- | x 
isVal E.Var{}     = True
-- | c
isVal E.Unit{}    = True
isVal E.Int{}     = True
isVal E.Float{}   = True
isVal E.Char{}    = True
isVal E.String{}  = True
-- | λm x:T . e
isVal E.Abs{}     = True
-- | Λa:κ . v
isVal E.TypeAbs{} = True
-- | {l=v_l}_l∈L 
isVal E.Pair{}    = True
-- | l v -- TODO
-- | select l
isVal (E.App _ (E.Var x) _) | x == mkSelect defaultSpan = True
-- | send [T]
isVal (E.TypeApp _ (E.Var x) _) | x == mkSend defaultSpan = True
-- | send [T] v
isVal (E.App _ (E.TypeApp _ (E.Var x) _) v) | x == mkSend defaultSpan = isVal v
-- | send [T] v [U]
isVal (E.TypeApp _ (E.App _ (E.TypeApp _ (E.Var x) _) v) _) | x == mkSend defaultSpan = isVal v
-- | receive [T]  
isVal (E.TypeApp _ (E.Var x) _) | x == mkReceive defaultSpan = True
-- | receive [T] [U]
isVal (E.TypeApp _ (E.TypeApp _ (E.Var x) _) _) | x == mkReceive defaultSpan = True
isVal (E.App _ (E.Var x) _) | x == mkClose defaultSpan = True
-- | otherwise
isVal _ = False
