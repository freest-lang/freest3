{- |
Module      :  Syntax.Show
Description :  The show module
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

Converting AST terms to strings.
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
import qualified Validation.Substitution as Subs (unfold) -- no renaming
import qualified Data.Map.Strict as Map
import           Data.List (intersperse, intercalate)
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
  | isDigit (head s) = dropWhile (\x -> isDigit x || x == '#') s
  | otherwise        = s
  where s = intern v
-- showVar = intern -- for testing purposes

-- Kinds

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

instance Show TypeVarBind where
  show (TypeVarBind _ a k) = show a
  -- show (TypeVarBind _ a k) = show a ++ ":" ++ show k

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
  -- show = showType 4
  show = showType 44 -- for testing purposes

showType :: Int -> Type -> String
  -- Non-recursive cases
showType _ (Basic _ b)      = show b
showType _ (Skip _)         = "Skip"
showType _ (TypeVar _ x)    = show x
showType _ (Message _ p b)  = show p ++ show b
showType _ (TypeName _ x)   = show x
  -- Depth reached
showType 0 _ = ".."
  -- Functional types
showType i (Fun _ m t u)    = "(" ++ showType (i-1) t ++ showArrow m ++ showType (i-1) u ++ ")"
showType i (PairType _ t u) = "(" ++ showType (i-1) t ++ ", " ++ showType (i-1) u ++ ")"
showType i (Datatype _ m)   = "[" ++ showDatatype i m ++ "]"
  -- Session types
showType i (Semi _ t u)     = "(" ++ showType (i-1) t ++ ";" ++ showType (i-1) u ++ ")"
showType i (Choice _ v m)   = showChoiceView v ++ "{" ++ showChoice i m ++ "}"
-- showType i t@(Rec _ _ _)    = showType (i-1) (Subs.unfold t)
showType i (Rec _ xk t)     = "(rec " ++ show xk ++ "." ++ showType (i-1) t ++ ")" -- for testing purposes
  -- Type operators
showType i (Dualof _ t)     = "(dualof " ++ showType (i-1) t ++ ")"
  
showDatatype :: Int -> TypeMap -> String
showDatatype i m = concat $ intersperse " | " $
  Map.foldrWithKey (\c t acc -> (show c ++ showAsSequence t) : acc ) [] m
  where
  showAsSequence :: Type -> String
  showAsSequence (Fun _ _ t u) = " " ++ showType (i-1) t ++ showAsSequence u
  showAsSequence _ = ""

showChoice :: Int -> TypeMap -> String
showChoice i m = concat $ intersperse ", " $
  Map.foldrWithKey (\c t acc -> (show c ++ ": " ++ showType (i-1) t) : acc) [] m

-- Type Schemes

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)
