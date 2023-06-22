{- |
Module      :  Syntax.Expressions
Description :  The expressions in the language
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Expression
  ( Exp(..)
  , FieldMap
  , Pattern(..)
  , FieldList
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K ( Kind )
import qualified Syntax.Type                   as T
import qualified Data.Map.Strict               as Map

data Exp =
  -- Basic values
    Unit Span
  | Int Span Int
  | Char Span Char
  | String Span String
  -- Variable
  | Var Span Variable
  -- Abstraction intro and elim
  | Abs Span Multiplicity (Bind T.Type Exp)        -- λ x:T -> e, λ x:T 1-> e
  | App Span Exp Exp     -- e1 e2
  -- Pair intro and elim
  | Pair Span Exp Exp
  | BinLet Span Variable Variable Exp Exp
  -- Datatype elim
  | Case    Span Exp FieldMap
  | CasePat Span Exp FieldList                       -- for pattern elimination
  -- Type Abstraction intro and elim
  | TypeAbs Span (Bind K.Kind Exp)   -- Λ a:k => e
  | TypeApp Span Exp T.Type     -- e[T]
  -- Boolean elim
  | Cond Span Exp Exp Exp
  -- Let
  | UnLet Span Variable Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)

instance Default (Bind T.Type Exp) where
  omission p = Bind p (omission p) (T.unit p) (Unit p)

type FieldMap  = Map.Map Variable ([Variable], Exp)
type FieldList = [([Pattern], Exp)]

data Pattern = PatVar  Variable           -- Variable   name
             | PatCons Variable [Pattern] -- Construtor name patterns

instance Located Exp where
  getSpan (Unit p             ) = p
  getSpan (Int p _            ) = p
  getSpan (Char p _           ) = p
  getSpan (String p _         ) = p
  getSpan (Var p _            ) = p
  getSpan (Abs p _ _          ) = p
  getSpan (App p _ _          ) = p
  getSpan (Pair p _ _         ) = p
  getSpan (BinLet p _ _ _ _   ) = p
  getSpan (Case  p _ _        ) = p
  getSpan (CasePat  p _ _     ) = p
  getSpan (TypeAbs p _        ) = p
  getSpan (TypeApp p _ _      ) = p
  getSpan (Cond p _ _ _       ) = p
  getSpan (UnLet p _ _ _      ) = p

  setSpan (Unit _              ) s = Unit s 
  setSpan (Int _ i             ) s = Int s i
  setSpan (Char _ c            ) s = Char s c
  setSpan (String _ str        ) s = String s str
  setSpan (Var _ v             ) s = Var s v
  setSpan (Abs _ m b           ) s = Abs s m b
  setSpan (App _ e1 e2         ) s = App s e1 e2
  setSpan (Pair _ e1 e2        ) s = Pair s e1 e2
  setSpan (BinLet _ v1 v2 e1 e2) s = BinLet s v1 v2 e1 e2
  setSpan (Case  _ e fm        ) s = Case s e fm
  setSpan (CasePat  _ e fl     ) s = CasePat s e fl
  setSpan (TypeAbs _ b         ) s = TypeAbs s b
  setSpan (TypeApp _ e t       ) s = TypeApp s e t
  setSpan (Cond _ e1 e2 e3     ) s = Cond s e1 e2 e3
  setSpan (UnLet _ v e1 e2     ) s = UnLet s v e1 e2
