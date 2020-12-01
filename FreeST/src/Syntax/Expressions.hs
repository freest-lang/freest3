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
  | TypeAbs Pos KindBind Expression     -- λ a:k => e
  | TypeApp Pos Expression Type         -- e[T]
  -- | TypeApp Pos ProgVar Type      
  -- | TypeApp Pos ProgVar [Type]
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
  pos (Unit p)              = p
  pos (Integer p _)         = p
  pos (Character p _)       = p
  pos (Boolean p _)         = p
  pos (ProgVar p _)         = p
  pos (Abs p _ _ _)         = p
  pos (UnLet p _ _ _)       = p
  pos (App p _ _)           = p
  pos (TypeApp p _ _)       = p
  pos (TypeAbs p _ _)       = p
  pos (Conditional p _ _ _) = p
  pos (Pair p _ _)          = p
  pos (BinLet p _ _ _ _)    = p
  pos (New p _ _)           = p
  pos (Select p _)          = p
  pos (Match p _ _)         = p
  pos (Case p _ _)          = p
