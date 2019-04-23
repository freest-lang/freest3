{- |
Module      :  Syntax.Expressions
Description :  The language expressions
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

In this module we provide the language expressions. We also define the expression
environment which contains the named functions in a program.
-}

module Syntax.Expressions
( Expression(..)
, FieldMap
, ExpEnv
) where

import           Syntax.Types (Type)
import           Syntax.ProgramVariables
import           Syntax.Base
import           Data.List (intersperse, intercalate)
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
  | Lambda Pos Multiplicity ProgVar Type Expression
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos Expression Expression
  | BinLet Pos ProgVar ProgVar Expression Expression
  -- Datatype elim
  | Case Pos Expression FieldMap
  -- Type application
  | TypeApp Pos ProgVar [Type]
  -- Boolean elim
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos ProgVar Expression Expression
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression
  | Receive Pos Expression
  | Select Pos ProgVar Expression
  | Match Pos Expression FieldMap

type FieldMap  = Map.Map ProgVar ([ProgVar], Expression)

-- The definitions of the named functions in a program
type ExpEnv = Map.Map ProgVar Expression

instance Position Expression where
  position (Unit p)              = p
  position (Integer p _)         = p
  position (Character p _)       = p
  position (Boolean p _)         = p
  position (ProgVar p _)         = p
  position (Lambda p _ _ _ _)    = p
  position (UnLet p _ _ _)       = p
  position (App p _ _)           = p
  position (TypeApp p _ _)       = p
  position (Conditional p _ _ _) = p
  position (Pair p _ _)          = p
  position (BinLet p _ _ _ _)    = p
  position (New p _)             = p
  position (Send p _)            = p
  position (Receive p _ )        = p
  position (Select p _ _)        = p
  position (Match p _ _)         = p
  position (Fork p _)            = p
  position (Case p _ _)          = p
