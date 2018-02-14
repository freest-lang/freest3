module Terms.Terms (
    Program (..),
    Expression (..),
    ExpEnv,
    TypeEnv,
    Args
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
type TypeEnv = Map.Map TypeVar Type
type ExpEnv = Map.Map TermVar (Args, Expression)

type Op = String
data Expression =
    BasicTerm BasicType
  | IntApp Op Expression Expression
  | BoolApp Op Expression Expression
  | UnBoolApp Op Expression
  | Var Id
  -- | Elim Expression Expression
  deriving (Show,Ord,Eq)
