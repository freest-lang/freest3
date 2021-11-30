{- |
Module      :  Syntax.Types
Description :  The types in FreeST.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the syntax for types.
-}

module Syntax.Type
  ( Type(..)
  , TypeMap
  , Polarity(..)
  , View(..)
--  , Multiplicity(..)
  )
where

import           Syntax.Base
import           Syntax.TypeVariable            ( TypeVar )
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Kind                   as K
import qualified Data.Map.Strict               as Map

data Polarity = Out | In deriving Eq
data View = External | Internal deriving Eq

data Type =
  -- Functional Types
    Int Pos
  | Char Pos
  | Bool Pos
  | String Pos
  | Unit Pos
  | Arrow Pos Multiplicity Type Type
  | Pair Pos Type Type
  | Variant Pos TypeMap
  -- Session Types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity Type
  | Choice Pos View TypeMap
  -- Polymorphism and recursive types
  | Forall Pos (K.Bind Type)   -- ∀k . T, Universal type
  | Rec Pos (K.Bind Type)      -- μ a:k . T, Recursive type
  | Var Pos TypeVar
  -- Type operators
  | Dualof Pos Type
  | CoVar Pos TypeVar
  | Abs Pos (K.Bind Type)       -- λ a:k -> T, Operator abstraction
  | App Pos Type Type           -- T T       , Operator application

type TypeMap = Map.Map ProgVar Type

instance Position Type where
  pos (Int  p       ) = p
  pos (Char p       ) = p
  pos (Bool p       ) = p
  pos (String p     ) = p
  pos (Unit p       ) = p
  pos (Arrow p _ _ _) = p
  pos (Pair p _ _   ) = p
  pos (Variant p _  ) = p
  pos (Skip p       ) = p
  pos (Semi p _ _   ) = p
  pos (Message p _ _) = p
  pos (Choice p _ _ ) = p
  pos (Forall p _   ) = p
  pos (Rec p _      ) = p
  pos (Var p _      ) = p
  pos (Dualof p _   ) = p
  pos (CoVar p _    ) = p
  pos (Abs p _      ) = p
  pos (App p _ _    ) = p


instance Default Type where
  omission = Int
