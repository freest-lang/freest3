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
    Unit Pos
  | Int Pos Int
  | Char Pos Char
  | Bool Pos Bool
  | String Pos String
  -- Variable
  | Var Pos Variable
  -- Abstraction intro and elim
  | Abs Pos Multiplicity (Bind T.Type Exp)        -- λ x:T -> e, λ x:T -o e
  | App Pos Exp Exp     -- e1 e2
  -- Pair intro and elim
  | Pair Pos Exp Exp
  | BinLet Pos Variable Variable Exp Exp
  -- Datatype elim
  | Case Pos Exp FieldMap
  -- Type Abstraction intro and elim
  | TypeAbs Pos (Bind K.Kind Exp)   -- Λ a:k => e
  | TypeApp Pos Exp T.Type     -- e[T]
  -- Boolean elim
  | Cond Pos Exp Exp Exp
  -- Let
  | UnLet Pos Variable Exp Exp -- TODO: Derived; eliminate? If yes, which is type for the ProgVar? (cf. Abs)
  -- Session types
  | New Pos T.Type T.Type

instance Default (Bind T.Type Exp) where
  omission p = Bind p (omission p) (T.Unit p) (Unit p)

type FieldMap = Map.Map Variable ([Variable], Exp)

instance Position Exp where
  pos (Unit p             ) = p
  pos (Int p _            ) = p
  pos (Char p _           ) = p
  pos (Bool p _           ) = p
  pos (String p _         ) = p
  pos (Var p _            ) = p
  pos (Abs p _ _          ) = p
  pos (UnLet p _ _ _      ) = p
  pos (App p _ _          ) = p
  pos (TypeApp p _ _      ) = p
  pos (TypeAbs p _        ) = p
  pos (Cond p _ _ _       ) = p
  pos (Pair p _ _         ) = p
  pos (BinLet p _ _ _ _   ) = p
  pos (New p _ _          ) = p
  pos (Case  p _ _        ) = p
