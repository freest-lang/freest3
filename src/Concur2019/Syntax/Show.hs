{- |
Module      :  Syntax.Show
Description :  The show module
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

The show instances.
-}


module Syntax.Show
( showChoiceView
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map
import           Data.List (intersperse)
import           Data.Char (isDigit)

-- Positions (Base)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c

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
  | isDigit (head s) = tail $ dropWhile isDigit s
  | otherwise        = s
  where s = intern v

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

showChoiceView :: Polarity -> String
showChoiceView In  = "&"
showChoiceView Out = "+"

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
  show (Datatype _ m)   = "["++ showDatatype m ++"]"
  -- Session types
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Choice _ v m)   = showChoiceView v ++ "{" ++ showChoice m ++ "}"
  show (Rec _ x t)      = "(rec " ++ show x ++ ". " ++ show t ++ ")"
  -- Functional or session
  show (TypeVar _ x)    = show x
  -- Type operators
  show (Dualof _ s)     = "(dualof " ++ show s ++ ")"
  show (TypeName _ x)   = show x
  
showDatatype :: TypeMap -> String
showDatatype m = concat $ intersperse " | " (map showAssoc (Map.assocs m))
  where
  showAssoc :: (ProgVar, Type) -> String
  showAssoc (c, t) = show c ++ showAsSequence t
  showAsSequence :: Type -> String
  showAsSequence (Fun _ _ t u) = " " ++ show t ++ showAsSequence u
  showAsSequence _ = ""

showChoice :: TypeMap -> String
showChoice m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where
  showAssoc :: (ProgVar, Type) -> String
  showAssoc (c, t) = show c ++ ": " ++ show t

-- Type Schemes

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)
