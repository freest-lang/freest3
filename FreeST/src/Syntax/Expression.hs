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
  , FieldMapP
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
  | Bool Span Bool
  | String Span String
  -- Variable
  | Var Span Variable
  -- Abstraction intro and elim
  | Abs Span Multiplicity (Bind T.Type Exp)        -- λ x:T -> e, λ x:T -o e
  | App Span Exp Exp     -- e1 e2
  -- Pair intro and elim
  | Pair Span Exp Exp
  | BinLet Span Variable Variable Exp Exp
  -- Datatype elim
  | Case  Span Exp FieldMap
  | CaseP Span Exp FieldMapP                       -- for pattern elimination
  -- Type Abstraction intro and elim
  | TypeAbs Span (Bind K.Kind Exp)   -- Λ a:k => e
  | TypeApp Span Exp T.Type     -- e[T]
  -- Boolean elim
  | Cond Span Exp Exp Exp
  -- Let
  | UnLet Span Variable Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)
  -- Session types
  | New Span T.Type T.Type

instance Default (Bind T.Type Exp) where
  omission p = Bind p (omission p) (T.Unit p) (Unit p)

type FieldMap  = Map.Map Variable ([Variable], Exp)
type FieldMapP = [([Pattern], Exp)]

data Pattern = V Variable           -- Variable   name
             | C Variable [Pattern] -- Construtor name patterns
             | L Exp                -- Literal    content

-- TODOX remove
instance Show Pattern where
  show (V v)    = "V " ++ intern v
  show (C v ps) = "C " ++ intern v ++ show ps
  show (L e)    = "L VALUE"

instance Located Exp where
  getSpan (Unit p             ) = p
  getSpan (Int p _            ) = p
  getSpan (Char p _           ) = p
  getSpan (Bool p _           ) = p
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
  getSpan (New p _ _          ) = p
  getSpan (Case  p _ _        ) = p
