module Syntax.Terms
  ( Expression(..)
  , ExpEnv
  , VarEnv
  , TypeEnv
  , ConstructorEnv
  , TermVar
  , Params
  , CaseMap
  , MatchMap
  , Pos
  ) where

import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Types


type TermVar = String

type Params = [TermVar]

type VarEnv = Map.Map TermVar TypeScheme
-- type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Params, Expression)

type TypeEnv = Map.Map TypeVar Type
-- type TypeEnv = Map.Map TypeVar (Kind, Type)

type ConstructorEnv = Map.Map TypeVar TypeScheme
-- type ConstructorEnv = Map.Map TypeVar Type

data TypeVarBind = TypeVar Kind
-- data TypeScheme = Functional Type | Scheme TypeVarBind TypeScheme

type Pos = (Int, Int)
-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

-- TODO: Join
type MatchMap = Map.Map TermVar (TermVar, Expression)
type CaseMap = Map.Map TermVar (Params, Expression)

data Expression
  -- Basic expressions
  = Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variables
  | Variable Pos TermVar
  | UnLet Pos TermVar Expression Expression
  -- Aplication
  | App Pos Expression Expression
  | TypeApp Pos Expression [Type]
  -- Conditional
  | Conditional Pos Expression Expression Expression
  -- Pairs
  | Pair Pos Expression Expression
  | BinLet Pos TermVar TermVar Expression Expression
  -- Session types
  | New Pos Type
  | Send Pos Expression Expression
  | Receive Pos Expression 
  | Select Pos TermVar Expression
  | Match Pos Expression MatchMap
  -- Fork
  | Fork Pos Expression
  -- Datatypes
  | Constructor Pos TermVar
  | Case Pos Expression CaseMap
   deriving (Eq, Ord)

-- -- TODO: ??
-- instance Show Expression where
--   show (Variable _ x) = x
--   show e = ""

