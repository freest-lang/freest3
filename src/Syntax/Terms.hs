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
  , getEPos
  ) where

import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Types


type TermVar = String

type Params = [TermVar]

type VarEnv = Map.Map TermVar (Pos, TypeScheme)
-- type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Pos, Params, Expression)

type TypeEnv = Map.Map TypeVar Type
-- type TypeEnv = Map.Map TypeVar (Kind, Type)

type ConstructorEnv = Map.Map TypeVar (Pos, TypeScheme)
-- type ConstructorEnv = Map.Map TypeVar Type

data TypeVarBind = TypeVar Kind
-- data TypeScheme = Functional Type | Scheme TypeVarBind TypeScheme


-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

-- TODO: Join
type MatchMap = Map.Map TermVar (TermVar, Expression)
type CaseMap  = Map.Map TermVar (Params, Expression)
type VarDef   = (Pos,TermVar) 

data Expression
  -- Basic expressions
  = Unit Pos
  | Integer Pos Int
  | Character Pos Char
  | Boolean Pos Bool
  -- Variables
  | Variable Pos TermVar
  | UnLet Pos VarDef Expression Expression
  -- Aplication
  | App Pos Expression Expression
  | TypeApp Pos Expression [Type]
  -- Conditional
  | Conditional Pos Expression Expression Expression
  -- Pairs
  | Pair Pos Expression Expression
  | BinLet Pos VarDef VarDef Expression Expression
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
