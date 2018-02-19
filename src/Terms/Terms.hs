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
  | App Op Expression Expression
  | UnApp Op Expression
  | ExpPair Expression Expression
  | Let Id Id Expression Expression
  | Var Id
  deriving (Show,Ord,Eq)
