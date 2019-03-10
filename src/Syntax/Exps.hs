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
  , Params
  , Param(..)
  , CaseMap
  , MatchMap
  , getEPos
  ) where

import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Types

type TermVar = String

data Param = Param {paramPos :: Pos, param :: TermVar}

instance Eq Param where
  b == c = param b == param c

instance Ord Param where
  (Param _ x) `compare` (Param _ y) = x `compare` y

instance Show Param where
  show p = param p

type Params = [Param] -- Params is a semantic notion, not syntatic - eliminate, use [TermVar]

data TypeVarBind = TypeVar Kind

-- TODO: Join
type MatchMap = Map.Map TermVar (Param, Expression)
type CaseMap  = Map.Map TermVar (Params, Expression)
type VarDef   = (Pos, TermVar) -- TODO: porque é que este tem Pos e o Multiplicity e o PreKind não?

data Expression =
  -- Basic values
    Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variable
  | Variable Pos TermVar
  -- Abstraction intro and elim
  {- Lam Pos Multiplicity TermVar Exp -}
  | App Pos Expression Expression
  -- Pair intro and elim
  | Pair Pos {- Multiplicity -} Expression Expression
  | BinLet Pos VarDef VarDef Expression Expression
  -- Datatype intro and elim
  | Constructor Pos TermVar
  | Case Pos Expression CaseMap
  -- Type application
  | TypeApp Pos TermVar [Type] -- Expression -> TermVar
  -- Conditional
  | Conditional Pos Expression Expression Expression
  -- Let
  | UnLet Pos VarDef Expression Expression -- Derived; eliminate?
  -- Fork
  | Fork Pos Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression Expression -- Omit the last Expression
  | Receive Pos Expression 
  | Select Pos TermVar Expression
  | Match Pos Expression MatchMap
   deriving (Eq, Ord, Show)

getEPos :: Expression -> Pos
getEPos (Unit p) = p
getEPos (Integer p _) = p
getEPos (Character p _) = p
getEPos (Boolean p _) = p
getEPos (Variable p _) = p
getEPos (UnLet p _ _ _) = p
getEPos (App p _ _) = p
getEPos (TypeApp p _ _) = p
getEPos (Conditional p _ _ _) = p
getEPos (Pair p _ _) = p
getEPos (BinLet p _ _ _ _) = p
getEPos (New p _) = p
getEPos (Send p _ _) = p
getEPos (Receive p _ ) = p
getEPos (Select p _ _) = p
getEPos (Match p _ _) = p
getEPos (Fork p _) = p
getEPos (Constructor p _) = p
getEPos (Case p _ _) = p
