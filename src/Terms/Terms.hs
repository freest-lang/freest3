module Terms.Terms (
  Expression (..)
, ExpEnv
, VarEnv
, TypeEnv
, ConstructorEnv
, TermVar
, Args
) where

import qualified Data.Map.Strict as Map
import           Types.Types
import           Types.Kinds

type TermVar = String
type Args = [TermVar]
type VarEnv = Map.Map TermVar Type
type ExpEnv = Map.Map TermVar (Args, Expression)
type TypeEnv = Map.Map TypeVar Type
-- type TypeEnv = Map.Map TypeVar (Kind, Type)
-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

type ConstructorEnv = Map.Map Constructor Type

data Expression =
  -- Basic expressions
    Unit
  | Integer Int
  | Character Char
  | Boolean Bool
  -- Variables
  | Variable TermVar
  -- Aplication
  | Application Expression Expression
  -- Conditional
  | Conditional Expression Expression Expression
  -- Pairs
  | Pair Expression Expression
  | Let TermVar TermVar Expression Expression
  -- Session types
  | New Type
  | Send Expression Expression
  | Receive Expression
  | Select Constructor Expression
  -- Branch - overloaded with Case
  -- Fork
  | Fork Expression
  -- Datatypes
  | Value Constructor                                               -- TODO
  | Case Expression (Map.Map Constructor ([TermVar], Expression))   -- TODO
  deriving Show -- TODO: write a proper show
