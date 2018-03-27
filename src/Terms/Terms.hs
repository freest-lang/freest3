module Terms.Terms
  ( Expression(..)
  , ExpEnv
  , VarEnv
  , TypeEnv
  , ConstructorEnv
  , TermVar
  , Params
  , CaseMap
  , showParams
  ) where

import qualified Data.Map.Strict as Map
import           Types.Kinds
import           Types.Types
import           Data.List

type TermVar = String

type Params = [TermVar]

type VarEnv = Map.Map TermVar Type

type ExpEnv = Map.Map TermVar (Params, Expression)

type TypeEnv = Map.Map TypeVar Type
-- type TypeEnv = Map.Map TypeVar (Kind, Type)

type ConstructorEnv = Map.Map TypeVar Type

data TypeVarBind = TypeVar Kind
data TypeScheme = Functional Type | Scheme TypeVarBind TypeScheme

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
  | Pair Expression Expression -- TODO: Express as application
  | Let TermVar TermVar Expression Expression
  -- Session types
  | New Type
  | Send Expression Expression -- TODO: Express as application
  | Receive Expression -- TODO: Express as application
  | Select TermVar Expression
  | Match Expression (Map.Map TermVar (TypeVar, Expression))
  -- Branch - overloaded with Case
  -- Fork
  | Fork Expression -- TODO: Express as application
  -- Datatypes
  | Constructor TermVar
  | Case Expression CaseMap
--  deriving Show
-- ("parseCase",([],Case (Application (Application (Variable "(+)") (Integer 2)) (Integer 2))
--  (fromList [("C",(["a"],Integer 23)),("D",(["a"],Integer 24)),("E",(["a"],Integer 25))])))

instance Show Expression where
  show  Unit               = "()"
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
  show (Case e1 cm) = "case " ++ show e1 ++ " of\n  " ++ (showCaseMap cm)

showApplication :: Expression -> Expression -> String
showApplication (Application (Variable ('(':op:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ show e3 ++ ")"
showApplication (Application (Variable ('(':op:op2:")")) e2) e3  = "(" ++ show e2 ++ [op] ++ [op2] ++ show e3 ++ ")"
-- showApplication (Application (Variable "(+)") e2) e3  = "(" ++ show e2 ++ " + " ++ show e3 ++ ")"
-- showApplication (Application (Variable "(-)") e2) e3  = "(" ++ show e2 ++ " - " ++ show e3 ++ ")"
-- showApplication (Application (Variable "(/)") e2) e3  = "(" ++ show e2 ++ " / " ++ show e3 ++ ")"
-- showApplication (Application (Variable "(*)") e2) e3  = "(" ++ show e2 ++ " * " ++ show e3 ++ ")"
-- showApplication (Application (Variable "(==)") e2) e3 = "(" ++ show e2 ++ " == " ++ show e3 ++ ")"
-- showApplication (Application (Variable "(>=)") e2) e3 = "(" ++ show e2 ++ " == " ++ show e3 ++ ")"
showApplication (Variable "negate") e2                = "-" ++ show e2
showApplication (Application (Variable "div") e2) e3  = show e2 ++ " `div` " ++ show e3
showApplication (Application (Variable "rem") e2) e3  = show e2 ++ " `rem` " ++ show e3
-- TODO others
-- showApplication (Application e1 e2) e3                = show e2 ++ " " ++ show e1  ++ " " ++ show e3
showApplication e1 e2                                 = show e1 ++ " " ++ show e2 
showLet :: TermVar -> TermVar -> Expression -> Expression -> String
showLet tv1 tv2 e1 e2 = "let " ++ tv1 ++ " = " ++ showFst e1 ++ "\n" ++
                        "let " ++ tv2 ++ " = " ++ showSnd e1 ++ " in " ++ show e2

showFst :: Expression -> String
showFst (Pair e1 _) = show e1
showFst e           = show e

showSnd :: Expression -> String
showSnd (Pair _ e2) = show e2
showSnd e           = show e

-- type CaseMap = Map.Map TermVar (Params, Expression)
showCaseMap :: CaseMap -> String
showCaseMap = Map.foldlWithKey (\acc tv (params, e) -> acc ++ tv ++ " " ++
                                 showParams params ++ "-> " ++ show e ++ "\n  ") ""  

showParams :: Params -> String
showParams as
  | null as = ""
  | otherwise = " " ++ (intercalate " " as)

