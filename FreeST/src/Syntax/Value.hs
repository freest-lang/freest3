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
isVal (E.App _ (E.Var p x) _) | x == mkSelect p = True
-- | send [T]
isVal (E.TypeApp _ (E.Var p x) _) | x == mkSend p = True
-- | send [T] v
isVal (E.App _ (E.TypeApp _ (E.Var p x) _) v) | x == mkSend p = isVal v
-- | send [T] v [U]
isVal (E.TypeApp _ (E.App _ (E.TypeApp _ (E.Var p x) _) v) _) | x == mkSend p = isVal v
-- | receive [T]  
isVal (E.TypeApp _ (E.Var p x) _) | x == mkReceive p = True
-- | receive [T] [U]
isVal (E.TypeApp _ (E.TypeApp _ (E.Var p x) _) _) | x == mkReceive p = True
isVal (E.App _ (E.Var p x) _) | x == mkClose p = True
-- | otherwise
isVal _ = False
