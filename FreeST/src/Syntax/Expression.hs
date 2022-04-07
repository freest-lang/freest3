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
  | Case Span Exp FieldMap
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

type FieldMap = Map.Map Variable ([Variable], Exp)

instance Position Exp where
  pos (Unit p             ) = startPos p
  pos (Int p _            ) = startPos p
  pos (Char p _           ) = startPos p
  pos (Bool p _           ) = startPos p
  pos (String p _         ) = startPos p
  pos (Var p _            ) = startPos p
  pos (Abs p _ _          ) = startPos p
  pos (UnLet p _ _ _      ) = startPos p
  pos (App p _ _          ) = startPos p
  pos (TypeApp p _ _      ) = startPos p
  pos (TypeAbs p _        ) = startPos p
  pos (Cond p _ _ _       ) = startPos p
  pos (Pair p _ _         ) = startPos p
  pos (BinLet p _ _ _ _   ) = startPos p
  pos (New p _ _          ) = startPos p
  pos (Case  p _ _        ) = startPos p

instance Spannable Exp where
  span (Unit p             ) = p
  span (Int p _            ) = p
  span (Char p _           ) = p
  span (Bool p _           ) = p
  span (String p _         ) = p
  span (Var p _            ) = p
  span (Abs p _ _          ) = p
  span (UnLet p _ _ _      ) = p
  span (App p _ _          ) = p
  span (TypeApp p _ _      ) = p
  span (TypeAbs p _        ) = p
  span (Cond p _ _ _       ) = p
  span (Pair p _ _         ) = p
  span (BinLet p _ _ _ _   ) = p
  span (New p _ _          ) = p
  span (Case  p _ _        ) = p
