{- |
Module      :  Show
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Show () where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map
import           Data.List (intersperse, intercalate)
import           Data.Char (isDigit)

-- Multiplicities (Base)

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

showArrow :: Multiplicity -> String
showArrow Lin = " -o "
showArrow Un  = " -> "

-- Program Variables. Note: show should be aligned with the creation
-- of new variables; see Syntax.ProgramVariables

instance Show ProgVar where
  show x = showVar $ intern x

-- Type Variables. Note: show should be aligned with the creation
-- of new variables; see Syntax.TypeVariables

instance Show TypeVar where
  show x = showVar $ intern x

showVar :: String -> String
showVar id
  | isDigit (head id) = tail $ dropWhile (isDigit) id
  | otherwise         = id

-- Kinds

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

instance Show TypeVarBind where
  show (TypeVarBind _ a k) = show a ++ ":" ++ show k

-- Types

instance Show Polarity where
  show In  = "?"
  show Out = "!"

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

instance Show Type where
  -- Functional types
  show (Basic _ b)      = show b
  show (Fun _ m t u)    = "(" ++ show t ++ showArrow m ++ show u ++ ")"
  show (PairType _ t u) = "(" ++ show t ++ ", " ++ show u ++ ")"
  show (Datatype _ m)   = "["++ showTypeMap m ++"]"
  -- Session types
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Choice _ v m)   = showChoice v ++ "{" ++ showTypeMap m ++ "}"
  show (Rec _ x t)      = "(rec " ++ show x ++ ". " ++ show t ++ ")"
  -- Functional or session
  show (TypeVar _ x)    = show x
  -- Type operators
  show (Dualof _ s)     = "(dualof " ++ show s ++ ")"
  show (TypeName _ x)   = show x
  
showChoice :: Polarity -> String
showChoice In  = "&"
showChoice Out = "+"

showTypeMap :: TypeMap -> String
showTypeMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (b, v) = show b ++ ": " ++ show v

-- Type Schemes

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)

-- Expressions

instance Show Expression where
  show e = showExp e 4

showExp :: Expression -> Int -> String
  -- Basic values
showExp (Unit _) _  = "()"
showExp (Integer _ i) _ = show i
showExp (Character _ c) _ = show c
showExp (Boolean _ b) _  = show b
  -- Variable
showExp (ProgVar _ x) _  = show x
  -- Depth reached
showExp _ 0 = ".."
  -- Abstraction intro and elim
showExp (Lambda _ m b t e) i = "(\\" ++ show b ++ " : " ++ show t ++ showArrow m ++ (showExp e (i-1)) ++ ")"
showExp (App _ e1 e2) i = "(" ++ showExp e1 (i-1) ++ " " ++ showExp e2 (i-1) ++ ")"
  -- Pair intro and elim
showExp (Pair _ e1 e2) i = " (" ++ (showExp e1 (i-1)) ++ ", " ++ (showExp e1 (i-1)) ++ ")"
showExp (BinLet _ b1 b2 e1 e2) i = "(let " ++ show b1 ++ ", " ++ show b2 ++ " = " ++ showExp e1 (i-1) ++ " in " ++ showExp e2 (i-1) ++ ")"
  -- Datatype elim
showExp (Case _ e m) i = "case " ++ showExp e (i-1) ++ " of {" ++ showFieldMap m (i-1) ++ "}"
  -- Type application
showExp (TypeApp _ x ts) _ = show x ++ " [" ++ (intercalate " " (map show ts)) ++ "]"
  -- Boolean elim
showExp (Conditional _ e e1 e2) i = "if " ++ show e ++ " then " ++ showExp e1 (i-1) ++ " else " ++ showExp e2 (i-1)
  -- Let
showExp (UnLet _ b1 e1 e2) i = "(let " ++ show b1 ++ " = " ++ showExp e1 (i-1) ++ " in " ++ showExp e2 (i-1) ++ ")"
  -- Fork
showExp (Fork _ e) i = "fork " ++ (showExp e (i-1))
  -- Session types
showExp (New _ t) _ = "new " ++ show t
showExp (Send _ e) _ = "send " ++ show e
showExp (Receive _ e) _ = "receive " ++ show e
showExp (Select _ l e) i = "select " ++ show l ++ showExp e (i-1)
showExp (Match _ e m) i = "match " ++ show e ++ " with {" ++ showFieldMap m i ++ "}"

showFieldMap :: FieldMap -> Int -> String
showFieldMap m i = concat $ intersperse "; " (map showAssoc (Map.toList m))
  where showAssoc (b, (a,v)) = show b ++ " " ++ intercalate " " (map show a) ++ " -> " ++  (showExp v (i-1))
