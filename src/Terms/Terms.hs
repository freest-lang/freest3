module Terms.Terms (
    Program (..),
    Expression (..),
    ExpEnv,
    TypeEnv,
    Args
  ) where

import qualified Data.Map.Strict as Map
import           Types.Types


--TODO: review
type Args = [String]

data Program = -- TODO: Do we need this datatype?
    Empty
  | TypeDecl Id Type
  | FunDecl Id Args Expression
  -- | FunDecl Id Type Expression
  deriving Show


type TypeVar = String -- TODO: Should come from module Types

-- type ExpEnv = Map.Map TermVar (Type, Expression)
type TypeEnv = Map.Map TypeVar Type
type ExpEnv = Map.Map TermVar (Args, Expression)

type Op = String
{-
data Expression =
    BasicTerm BasicType
  | App Op Expression Expression
  -- | BoolApp Op Expression Expression
  -- | UnBoolApp Op Expression
  | UnApp Op Expression
  | ExpPair Expression Expression
  | Var Id
  -- | Elim Expression Expression
  deriving (Show,Ord,Eq)
-}

type TermVar = String

data Expression =
  -- Basic types
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
  | Select ConstructorName Expression
  -- Fork
  | Fork Expression
  -- Datatypes
  | Value Constructor
  | Case Expression (Map.Map (ConstructorName, [TermVar]) Expression)
  deriving Show -- TODO: write a proper show

