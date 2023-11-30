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
    Unit (Span Exp)
  | Int (Span Exp) Int
  | Float (Span Exp) Double
  | Char (Span Exp) Char
  | String (Span Exp) String
  -- Variable
  | Var (Span Exp) Variable
  -- Abstraction intro and elim
  | Abs (Span Exp) Multiplicity (Bind T.Type Exp)        -- λ x:T -> e, λ x:T 1-> e
  | App (Span Exp) Exp Exp     -- e1 e2
  -- Pair intro and elim
  | Pair (Span Exp) Exp Exp
  | BinLet (Span Exp) Variable Variable Exp Exp
  -- Datatype elim
  | Case    (Span Exp) Exp FieldMap
  | CasePat (Span Exp) Exp FieldList                       -- for pattern elimination
  -- Type Abstraction intro and elim
  | TypeAbs (Span Exp) (Bind K.Kind Exp)   -- Λ a:k => e
  | TypeApp (Span Exp) Exp T.Type     -- e[T]
  -- Boolean elim
  | Cond (Span Exp) Exp Exp Exp
  -- Let
  | UnLet (Span Exp) Variable Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)

instance Default (Bind T.Type Exp) where
  omission p = Bind (clearSource p) (omission (clearSource p)) (T.unit (clearSource p)) (Unit (clearSource p))

type FieldMap  = Map.Map Variable ([Variable], Exp)
type FieldList = [([Pattern], Exp)]

data Pattern = PatVar  Variable           -- Variable   name
             | PatCons Variable [Pattern] -- Construtor name patterns

instance Located Exp where
  getSpan (Unit p             ) = p
  getSpan (Int p _            ) = p
  getSpan (Float p _          ) = p
  getSpan (Char p _           ) = p
  getSpan (String p _         ) = p
  getSpan (Var p _            ) = p
  getSpan (Abs p _ _          ) = p
  getSpan (UnLet p _ _ _      ) = p
  getSpan (App p _ _          ) = p
  getSpan (TypeApp p _ _      ) = p
  getSpan (TypeAbs p _        ) = p
  getSpan (Cond p _ _ _       ) = p
  getSpan (Pair p _ _         ) = p
  getSpan (BinLet p _ _ _ _   ) = p
  getSpan (Case  p _ _        ) = p
  getSpan (CasePat  p _ _     ) = p

  setSpan s (Unit _              ) = Unit s 
  setSpan s (Int _ i             ) = Int s i
  setSpan s (Float _ f           ) = Float s f
  setSpan s (Char _ c            ) = Char s c
  setSpan s (String _ str        ) = String s str
  setSpan s (Var _ v             ) = Var s v
  setSpan s (Abs _ m b           ) = Abs s m b
  setSpan s (App _ e1 e2         ) = App s e1 e2
  setSpan s (Pair _ e1 e2        ) = Pair s e1 e2
  setSpan s (BinLet _ v1 v2 e1 e2) = BinLet s v1 v2 e1 e2
  setSpan s (Case  _ e fm        ) = Case s e fm
  setSpan s (CasePat  _ e fl     ) = CasePat s e fl
  setSpan s (TypeAbs _ b         ) = TypeAbs s b
  setSpan s (TypeApp _ e t       ) = TypeApp s e t
  setSpan s (Cond _ e1 e2 e3     ) = Cond s e1 e2 e3
  setSpan s (UnLet _ v e1 e2     ) = UnLet s v e1 e2

