module Terms.Terms
  ( Expression(..)
  , ExpEnv
  , VarEnv
  , TypeEnv
 -- , ConstructorEnv
  , TermVar
  , Params
  , CaseMap
  ) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types

type TermVar = String

type Params = [TermVar]

type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Params, Expression)

type TypeEnv = Map.Map TypeVar Type

-- type ConstructorEnv = Map.Map Constructor Type

-- type TypeEnv = Map.Map TypeVar (Kind, Type)
-- type ConstructorEnv = Map.Map Constructor [(Constructor, [Type])]

type CaseMap = Map.Map TermVar (Params, Expression)

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
  | Select TermVar Expression
  | Match Expression (Map.Map TermVar (TypeVar, Expression))
  -- Branch - overloaded with Case
  -- Fork
  | Fork Expression
  -- Datatypes
  | Constructor TermVar
  | Case Expression CaseMap
--  deriving (Show) -- TODO: write a proper show


-- ("parseCase",([],Case (Application (Application (Variable "(+)") (Integer 2)) (Integer 2))
--  (fromList [("C",(["a"],Integer 23)),("D",(["a"],Integer 24)),("E",(["a"],Integer 25))])))

instance Show Expression where
  show (Unit)              = "()"
  show (Integer i)         = show i
  show (Character c)       = show c
  show (Boolean b)         = show b
  show (Variable v)        = v
  show (Application e1 e2) = showApplication e1 e2
  show (Conditional e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Pair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Let tv1 tv2 e1 e2) = showLet tv1 tv2 e1 e2
  -- TODO...
  show (New t) = "New " ++ show t
  show (Send e1 e2) = "Send " ++ show e1 ++ " " ++ show e2
  show (Receive e1) = "Receive " ++ show e1
  show (Select tv e1) = "Select " ++ tv ++ " " ++ show e1 
--  show (Match e1 (Map.Map termVar (typeVar, es))) =
  show (Fork e1) = "Fork " ++ show e1
  show (Constructor tv) = tv
  -- show (Case e1 cm) = 

showApplication :: Expression -> Expression -> String
showApplication (Application (Variable "(+)") e2) e3  = show e2 ++ " + " ++ show e3
showApplication (Application (Variable "(-)") e2) e3  = show e2 ++ " - " ++ show e3
showApplication (Application (Variable "(/)") e2) e3  = show e2 ++ " / " ++ show e3
showApplication (Application (Variable "(*)") e2) e3  = show e2 ++ " * " ++ show e3
showApplication (Application (Variable "(==)") e2) e3 = show e2 ++ " == " ++ show e3
showApplication (Variable "negate") e2 = "-" ++ show e2
showApplication (Application e1 e2) e3 = show e2 ++ " " ++ show e1  ++ " " ++ show e3
showApplication e1 e2    = show e1 ++ " " ++ show e2 

showLet :: TermVar -> TermVar -> Expression -> Expression -> String
showLet tv1 tv2 e1 e2 = "let " ++ tv1 ++ " = " ++ show e1 ++ "\n" ++
                        "let " ++ tv2 ++ " = " ++ show e2
