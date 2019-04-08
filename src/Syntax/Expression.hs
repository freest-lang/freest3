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

module Syntax.Expression
( Expression(..)
, FieldMap
) where

import           Parse.Lexer (Position, Pos, position)
import           Syntax.Types (Type)
import           Syntax.Kinds (Multiplicity)
import           Syntax.Bind (PVar, PBind)
import qualified Data.Map.Strict as Map

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | ProgVar Pos PVar
  -- Abstraction intro and elim
  | Lambda Pos Multiplicity PBind Type Expression
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos {- Multiplicity -} Expression Expression
  | BinLet Pos PBind PBind Expression Expression
  -- Datatype intro and elim
  | Case Pos Expression FieldMap
  -- Type application
  | TypeApp Pos PVar [Type]
  -- Boolean elim
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos PBind Expression Expression -- TODO: Derived; eliminate?
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression
  | Receive Pos Expression 
  | Select Pos PVar Expression
  | Match Pos Expression FieldMap
  deriving (Eq, Ord, Show)

type FieldMap = Map.Map PBind Expression
-- type ExpMap  = Map.Map PBind ([PBind], Expression)

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
