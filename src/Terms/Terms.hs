module Terms.Terms (
    Program (..),
    Expression (..),
    ExpEnv,
    TypeEnv
  ) where

import Types.Types
import qualified Data.Map.Strict as Map


--TODO: review
type Args = [String]

data Program =
    Empty
  | TypeDecl Id Type
  | FunDecl Id Args Expression
  -- | FunDecl Id Type Expression
  deriving Show


type TermVar = String
type TypeVar = String

-- type ExpEnv = Map.Map TermVar (Type, Expression)
type ExpEnv = Map.Map TermVar Expression
type TypeEnv = Map.Map TypeVar Type

data Expression =
    BasicTerm BasicType
  | IntApp Expression Expression
  | BoolApp Expression Expression
  | UnBoolApp Expression
  | Elim Expression Expression
  deriving Show
