
module Terms.Terms
  ( Expression(..)
  , ExpEnv
  , VarEnv
  , TypeEnv
 -- , ConstructorEnv
  , TermVar
  , Args
  , CaseMap
  ) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types

type TermVar = String

type Args = [TermVar]

type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Args, Expression)

type TypeEnv = Map.Map TypeVar Type

-- type ConstructorEnv = Map.Map Constructor Type

-- type TypeEnv = Map.Map TypeVar (Kind, Type)
-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

type CaseMap = (Map.Map Constructor ([TermVar], Expression))

data Expression
  -- Basic expressions
  = Unit
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
  | Value Constructor 
  | Case Expression CaseMap
  deriving (Show) -- TODO: write a proper show


-- ("parseCase",([],Case (Application (Application (Variable "(+)") (Integer 2)) (Integer 2))
--  (fromList [("C",(["a"],Integer 23)),("D",(["a"],Integer 24)),("E",(["a"],Integer 25))])))

