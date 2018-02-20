module Terms.Terms (
--    Program (..)
  Expression (..)
, ExpEnv
, VarEnv
, TypeEnv
--, ConstructorEnv
, TermVar
, Args
) where

import qualified Data.Map.Strict as Map
import           Types.Types
import           Types.Kinds

{-
--TODO: review
type Args = [String]

data Program = -- TODO: Do we need this datatype?
    Empty
  | TypeDecl TypeVar Type
  | FunDecl TypeVar Args Expression
  -- | FunDecl TypeVar Type Expression
  deriving Show
-}

{-
type Op = String
data Expression =
    BasicTerm BasicType
  | App Op Expression Expression
  | UnApp Op Expression
  | ExpPair Expression Expression
  | Let TypeVar TypeVar Expression Expression
  | Var TypeVar
  deriving (Show,Ord,Eq)

-}

type TermVar = String
type Args = [TermVar]
type VarEnv = Map.Map TermVar Type
type ExpEnv = Map.Map TermVar (Args, Expression)
type TypeEnv = Map.Map TypeVar (Kind, Type)
--type ConstructorEnv

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
  | Conditional Expression Expression Expression -- TODO
  -- Pairs
  | Pair Expression Expression
  | Let TermVar TermVar Expression Expression
  -- Session types
  | New Type                      -- TODO
  | Send Expression Expression    -- TODO
  | Receive Expression            -- TODO
  | Select Constructor Expression -- TODO
  -- Branch - overloaded with Case
  -- Fork
  | Fork Expression                -- TODO
  -- Datatypes
  | Value Constructor               -- TODO
  | Case Expression (Map.Map Constructor ([TermVar], Expression)) -- TODO
  deriving Show -- TODO: write a proper show
