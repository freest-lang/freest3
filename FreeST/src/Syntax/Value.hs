module Syntax.Value
  ( isVal
  )
where

import           Syntax.Base
import qualified Syntax.Expression             as E

isVal :: E.Exp -> Bool
-- | x 
isVal E.Var{}     = True
-- | c
isVal E.Unit{}    = True
isVal E.Int{}     = True
isVal E.Char{}    = True
isVal E.Bool{}    = True
isVal E.String{}  = True
-- | λm x:T . e
isVal E.Abs{}     = True
-- | Λa:κ . v
isVal E.TypeAbs{} = True
-- | {l=v_l}_l∈L 
isVal E.Pair{}    = True
-- | l v -- TODO
-- | select l
isVal (E.App _ (E.Var p x) _) | x == mkVar p "select" = True
-- | send [T]
isVal (E.TypeApp _ (E.Var p x) _) | x == mkVar p "send" = True
-- | send [T] v
isVal (E.App _ (E.TypeApp _ (E.Var p x) _) v) | x == mkVar p "send" = isVal v
-- | send [T] v [U]
isVal (E.TypeApp _ (E.App _ (E.TypeApp _ (E.Var p x) _) v) _) | x == mkVar p "send" = isVal v
-- | receive [T]  
isVal (E.TypeApp _ (E.Var p x) _) | x == mkVar p "receive" = True
-- | receive [T] [U]
isVal (E.TypeApp _ (E.TypeApp _ (E.Var p x) _) _) | x == mkVar p "receive" = True
isVal (E.App _ (E.Var p x) _) | x == mkVar p "close" = True
-- | otherwise
isVal _ = False
