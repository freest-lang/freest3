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
  show = showVar

-- Type Variables. Note: show should be aligned with the creation
-- of new variables; see Syntax.TypeVariables

instance Show TypeVar where
  show = showVar

showVar :: Variable v => v -> String
showVar v
  -- | isDigit (head s) = tail $ dropWhile (isDigit) s
  -- | otherwise         = s
  -- where s = intern v
  = intern v -- Debug

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

showChoice :: Polarity -> String
showChoice In  = "&"
showChoice Out = "+"

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
  show = showExp 4

showExp :: Int -> Expression -> String
  -- Basic values
showExp _ (Unit _) = "()"
showExp _ (Integer _ i) = show i
showExp _ (Character _ c) = show c
showExp _ (Boolean _ b) = show b
  -- Variable
showExp _ (ProgVar _ x) = show x
  -- Depth reached
showExp 0 _ = ".."
  -- Abstraction intro and elim
showExp i (Lambda _ m b t e) = "(\\" ++ show b ++ " : " ++ show t ++ showArrow m ++ (showExp (i-1) e) ++ ")"
showExp i (App _ e1 e2) = "(" ++ showExp (i-1) e1 ++ " " ++ showExp (i-1) e2 ++ ")"
  -- Pair intro and elim
showExp i (Pair _ e1 e2) = "(" ++ showExp (i-1) e1 ++ ", " ++ showExp (i-1) e2 ++ ")"
showExp i (BinLet _ x y e1 e2) = "(let " ++ show x ++ ", " ++ show y ++ " = " ++ showExp (i-1) e1 ++ " in " ++ showExp (i-1) e2 ++ ")"
  -- Datatype elim
showExp i (Case _ e m) = "case " ++ showExp (i-1) e ++ " of {" ++ showFieldMap (i-1) m ++ "}"
  -- Type application
showExp _ (TypeApp _ x ts) = show x ++ " [" ++ (intercalate " " (map show ts)) ++ "]"
  -- Boolean elim
showExp i (Conditional _ e e1 e2) = "if " ++ show e ++ " then " ++ showExp (i-1) e1 ++ " else " ++ showExp (i-1) e2
  -- Let
showExp i (UnLet _ x e1 e2) = "(let " ++ show x ++ " = " ++ showExp (i-1) e1 ++ " in " ++ showExp (i-1) e2 ++ ")"
  -- Fork
showExp i (Fork _ e) = "fork " ++ showExp (i-1) e
  -- Session types
showExp _ (New _ t) = "new " ++ show t
showExp i (Send _ e) = "send " ++ showExp (i-1) e
showExp i (Receive _ e) = "receive " ++ showExp (i-1) e
showExp i (Select _ l e) = "select " ++ show l ++ showExp (i-1) e
showExp i (Match _ e m) = "match " ++ showExp (i-1) e ++ " with {" ++ showFieldMap (i-1) m ++ "}"

showFieldMap :: Int -> FieldMap -> String
showFieldMap i m = concat $ intersperse "; " (map showAssoc (Map.toList m))
  where showAssoc (b, (a,v)) = show b ++ " " ++ intercalate " " (map show a) ++ " -> " ++  (showExp (i-1) v)
