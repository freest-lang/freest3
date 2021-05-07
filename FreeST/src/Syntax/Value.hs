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
-- | λm x : T.e
isVal E.Abs{}     = True
-- | Λa : κ.v
isVal E.TypeAbs{} = True
-- | {l=vl}l∈L 
isVal E.Pair{}    = True
-- | select l
isVal (E.App _ (E.Var p x) _) | x == mkVar p "select" = True
-- | send[T]
isVal (E.TypeApp _ (E.Var p x) _) | x == mkVar p "send" = True
-- | send[T][T]
isVal (E.TypeApp _ (E.TypeApp _ (E.Var p x) _) _) | x == mkVar p "send" = True
-- | send[T][T]v
isVal (E.App _ (E.TypeApp _ (E.TypeApp _ (E.Var p x) _) _) e)
  | x == mkVar p "send" = isVal e
-- | receive[T]  
isVal (E.TypeApp _ (E.Var p x) _) | x == mkVar p "receive" = True
-- | receive[T]v
isVal (E.App _ (E.TypeApp _ (E.Var p x) _) e) | x == mkVar p "receive" = isVal e
isVal _ = False
