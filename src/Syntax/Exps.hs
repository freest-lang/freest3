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

module Syntax.Exps
( Expression(..)
, TermVar
-- , VarDef  
-- , Params
-- , Param(..)
, CaseMap
, MatchMap
) where

import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import qualified Data.Map.Strict as Map

type TermVar = Var -- = String

{-
data Param = Param {paramPos :: Pos, param :: TermVar}

instance Eq Param where
  b == c = param b == param c

instance Ord Param where
  (Param _ x) `compare` (Param _ y) = x `compare` y

instance Show Param where
  show p = param p
-}
--type Params = [Param] -- Params is a semantic notion, not syntatic - eliminate, use [TermVar]

-- data TypeVarBind = TypeVar Kind
 
-- TODO: Join these two
-- TODO: TermVar -> Bind
type MatchMap = Map.Map TermVar (Bind, Expression)
type CaseMap  = Map.Map TermVar ([Bind], Expression)
--type VarDef   = (Pos, TermVar) -- TODO: porque é que este tem Pos e o Multiplicity e o PreKind não?

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | Variable Pos TermVar
  -- Abstraction intro and elim
  {- Lam Pos Multiplicity Bind Exp -}
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos {- Multiplicity -} Expression Expression
  | BinLet Pos Bind Bind Expression Expression
  -- Datatype intro and elim
  | Constructor Pos TermVar
  | Case Pos Expression CaseMap
  -- Type application
  | TypeApp Pos TermVar [Type]
  -- Conditional
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos Bind Expression Expression -- TODO: Derived; eliminate?
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression Expression -- TODO: Omit the last Expression
  | Receive Pos Expression 
  | Select Pos TermVar Expression
  | Match Pos Expression MatchMap
   deriving (Eq, Ord, Show)

instance Position Expression where
  position (Unit p)              = p
  position (Integer p _)         = p
  position (Character p _)       = p
  position (Boolean p _)         = p
  position (Variable p _)        = p
  position (UnLet p _ _ _)       = p
  position (App p _ _)           = p
  position (TypeApp p _ _)       = p
  position (Conditional p _ _ _) = p
  position (Pair p _ _)          = p
  position (BinLet p _ _ _ _)    = p
  position (New p _)             = p
  position (Send p _ _)          = p
  position (Receive p _ )        = p
  position (Select p _ _)        = p
  position (Match p _ _)         = p
  position (Fork p _)            = p
  position (Constructor p _)     = p
  position (Case p _ _)          = p
