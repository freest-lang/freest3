{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Expressions
( Expression(..)
, FieldMap
, ExpEnv
) where

import           Syntax.Types (Type, TypeBind)
import           Syntax.Kinds (KindBind)
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | ProgVar Pos ProgVar
  -- Abstraction intro and elim
  | Abs Pos Multiplicity TypeBind Expression -- λ x:T -> e
  | App Pos Expression Expression            -- e1 e2
  -- Pair intro and elim
  | Pair Pos Expression Expression
  | BinLet Pos ProgVar ProgVar Expression Expression
  -- Datatype elim
  | Case Pos Expression FieldMap
  -- Type Abstraction intro and elim
  | TypeAbs Pos KindBind Expression     -- Λ a:k => e
  -- | TypeApp Pos Expression Type      -- e T, if that does not work, then e[T]. NEW, replace the below
  | TypeApp Pos ProgVar [Type]
  -- Boolean elim
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos ProgVar Expression Expression -- TODO: Derived; eliminate? If is which type for the ProgVar? (cf. Abs)
  -- Session types
  | New Pos Type Type
  | Select Pos ProgVar
  | Match Pos Expression FieldMap
   deriving Eq

type FieldMap  = Map.Map ProgVar ([ProgVar], Expression)

-- The definitions of the named functions in a program
type ExpEnv = Map.Map ProgVar Expression

instance Position Expression where
  position (Unit p)              = p
  position (Integer p _)         = p
  position (Character p _)       = p
  position (Boolean p _)         = p
  position (ProgVar p _)         = p
  position (Abs p _ _ _)         = p
  position (UnLet p _ _ _)       = p
  position (App p _ _)           = p
  position (TypeApp p _ _)       = p
  position (TypeAbs p _ _)       = p
  position (Conditional p _ _ _) = p
  position (Pair p _ _)          = p
  position (BinLet p _ _ _ _)    = p
  position (New p _ _)           = p
  position (Select p _)          = p
  position (Match p _ _)         = p
  position (Case p _ _)          = p
