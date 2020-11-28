{- |
Module      :  Syntax.Show
Description :  The show module
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

Converting AST terms to strings.
-}

module Parse.Unparser
  ( showChoiceView
  , showFieldMap
  )
where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict        as Map
import           Data.List              (intersperse, intercalate)
import           Data.Char              (isDigit)
import           Prelude                hiding (Left, Right) -- needed for Associativity

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

-- Program and Type Variables.

-- Note: show should be aligned with the creation of new variables;
-- see Syntax.ProgramVariables and Syntax.TypeVariables

instance Show ProgVar where
  show = showVar

instance Show TypeVar where
  show = showVar

showVar :: Variable v => v -> String
showVar = dropWhile (\c -> isDigit c || c == '#') . intern
-- showVar = intern -- for testing purposes

-- Kinds

instance Show PreKind where
  show Session    = "S"
  show Functional = "T"

instance Show Kind where
  show (Kind _ p m) = show p ++ show m

-- Kind binds

instance Show KindBind where
  show (KindBind _ a k) = show a ++ ":" ++ show k

-- Polarities

instance Show Polarity where
  show In  = "?"
  show Out = "!"

showChoiceView :: Polarity -> String
showChoiceView In  = "&"
showChoiceView Out = "+"

-- Basic types

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- Unparsing types and expressions

-- Norman Ramsey, Unparsing Expressions With Prefix and Postfix
-- Operators, Software—Practice and Experience, 1998.
-- https://www.cs.tufts.edu/~nr/pubs/unparse.ps

data Precedence =
  PMin
  -- Types and Expressions
  | PIn | PNew | PDot | PArrow | PSemi | PDualof | PApp
  | PMax deriving (Eq, Ord, Bounded)

data Associativity = Left | Right | NonAssoc deriving Eq

type Rator = (Precedence, Associativity)

type Fragment = (Rator, String)

inRator, newRator, dotRator, arrowRator, semiRator, dualofRator, appRator, minRator, maxRator :: Rator
inRator = (PIn, Right)       -- also else, match, case
newRator = (PNew, NonAssoc)
dotRator = (PDot, Right)
arrowRator = (PArrow, Right)
semiRator = (PSemi, Right)
dualofRator = (PDualof, Right)
appRator = (PApp, Left)
minRator = (minBound, NonAssoc)
maxRator = (maxBound, NonAssoc)
 
noparens :: Rator -> Rator -> Associativity -> Bool
noparens (pi, ai) (po, ao) side =
  pi > po || pi == po && ai == ao && ao == side

bracket :: Fragment -> Associativity -> Rator -> String
bracket (inner, image) side outer
  | noparens inner outer side = image
  | otherwise = "(" ++ image ++ ")"

class Unparse t where
  unparse :: t -> Fragment

-- Types

instance Show Type where
  show = snd . unparse

instance Unparse Type where
  unparse (Basic _ b) = (maxRator, show b)
  unparse (Skip _) = (maxRator, "Skip")
  unparse (TypeVar _ a) = (maxRator, show a)
  unparse (Message _ p b) = (maxRator, show p ++ show b)
  unparse (TypeName _ x) = (maxRator, show x)
  unparse (Fun _ m t u) = (arrowRator, l ++ showArrow m ++ r)
    where l = bracket (unparse t) Left arrowRator
          r = bracket (unparse u) Right arrowRator
  unparse (PairType _ t u) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
    where l = bracket (unparse t) Left minRator
          r = bracket (unparse u) Right minRator
  unparse (Datatype _ m) = (maxRator, "[" ++ showDatatype m ++ "]")
  unparse (Semi _ t u) = (semiRator, l ++ " ; " ++ r)
    where l = bracket (unparse t) Left semiRator
          r = bracket (unparse u) Right semiRator
  unparse (Choice _ v m) = (maxRator, showChoiceView v ++ "{" ++ showChoice m ++ "}")
  -- unparse (Forall _ b t) = (semiRator, l ++ ";" ++ r)
  unparse (Rec _ xk t) = (dotRator, "rec " ++ show xk ++ "." ++ s)
    where s = bracket (unparse t) Right dotRator
  unparse (Dualof _ t) = (dualofRator, "dualof " ++ s)
    where s = bracket (unparse t) NonAssoc dualofRator

showDatatype :: TypeMap -> String
showDatatype m = intercalate " | " $ Map.foldrWithKey
  (\c t acc -> (show c ++ showAsSequence t) : acc)
  []
  m
  where
    showAsSequence :: Type -> String
    showAsSequence (Fun _ _ t u) = " " ++ show t ++ showAsSequence u
    showAsSequence _             = ""

showChoice :: TypeMap -> String
showChoice m = intercalate ", " $ Map.foldrWithKey
  (\c t acc -> (show c ++ ": " ++ show t) : acc)
  []
  m
  
{-
instance Show Type where
  show = showType 4
--  show = showType 44 -- for testing purposes

showType :: Int -> Type -> String
  -- Non-recursive cases
showType _ (Basic _ b) = show b
showType _ (Skip _) = "Skip"
showType _ (TypeVar _ a) = show a
showType _ (Message _ p b) = show p ++ show b
showType _ (TypeName _ x) = show x
  -- Depth reached
showType 0 _               = ".."
  -- Functional types
showType i (Fun _ m t u) =
  "(" ++ showType (i - 1) t ++ showArrow m ++ showType (i - 1) u ++ ")"
showType i (PairType _ t u) =
  "(" ++ showType (i - 1) t ++ ", " ++ showType (i - 1) u ++ ")"
showType i (Datatype _ m) = "[" ++ showDatatype i m ++ "]"
  -- Session types
showType i (Semi _ t u) =
  "(" ++ showType (i - 1) t ++ ";" ++ showType (i - 1) u ++ ")"
showType i (Choice _ v m) = showChoiceView v ++ "{" ++ showChoice i m ++ "}"
showType i (Forall _ b t) = "∀" ++ show b ++ "=>" ++ showType (i - 1) t
showType i (Rec _ xk t) =
  "(rec " ++ show xk ++ "." ++ showType (i - 1) t ++ ")" -- for testing purposes
  -- Type operators
showType i (Dualof _ t) = "(dualof " ++ showType (i - 1) t ++ ")"

-- showNTupleType :: Int -> Type -> String
-- showNTupleType i (PairType _ t u) = showType (i - 1) t ++ showType (i - 1) u
-- showNTupleType i t                = showType i t

showDatatype :: Int -> TypeMap -> String
showDatatype i m = intercalate " | " $ Map.foldrWithKey
  (\c t acc -> (show c ++ showAsSequence t) : acc)
  []
  m
 where
  showAsSequence :: Type -> String
  showAsSequence (Fun _ _ t u) = " " ++ showType (i - 1) t ++ showAsSequence u
  showAsSequence _             = ""

showChoice :: Int -> TypeMap -> String
showChoice i m = intercalate ", " $ Map.foldrWithKey
  (\c t acc -> (show c ++ ": " ++ showType (i - 1) t) : acc)
  []
  m
-}

-- Type binds

instance Show TypeBind where
  show (TypeBind _ x t) = show x ++ ": " ++ show t

-- Expressions

instance Show Expression where
  show = snd . unparse

instance Unparse Expression where
  -- Basic values
  unparse (Unit _) = (maxRator, "()")
  unparse (Integer _ i) = (maxRator, show i)
  unparse (Character _ c) = (maxRator, show c)
  unparse (Boolean _ b) = (maxRator, show b)
  -- Variable
  unparse (ProgVar _ x) = (maxRator, show x)
  -- Abstraction intro and elim
  unparse (Abs _ m b e) = (arrowRator, "λ" ++ show b ++ showArrow m ++ s)
    where s = bracket (unparse e) Right arrowRator
  unparse (App _ e1 e2) = (appRator, l ++ " " ++ r)
    where l = bracket (unparse e1) Left appRator
          r = bracket (unparse e2) Right appRator
  -- Pair intro and elim
  unparse (Pair _ e1 e2) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
    where l = bracket (unparse e1) Left minRator
          r = bracket (unparse e2) Right minRator
  unparse (BinLet _ x y e1 e2) = (inRator, "let " ++ p ++ " = " ++ l ++ " in " ++ r)
    where p = "(" ++ show x ++ ", " ++ show y ++ ")"
          l = bracket (unparse e1) Left inRator
          r = bracket (unparse e2) Right inRator
  -- Datatype elim
  unparse (Case _ e m) = (inRator, "case " ++ s ++ " of {" ++ showFieldMap m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator
  -- Type Abstraction intro and elim
  unparse (TypeApp _ x ts) = (appRator, show x ++ " [" ++ unwords (map show ts) ++ "]")
  unparse (TypeAbs _ b e) = (arrowRator, "λ" ++ show b ++ "->" ++ s)
    where s = bracket (unparse e) Right arrowRator
  -- Boolean elim
  unparse (Conditional _ e1 e2 e3) = (inRator, "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3)
    where s1 = bracket (unparse e1) Left inRator
          s2 = bracket (unparse e2) NonAssoc inRator
          s3 = bracket (unparse e3) Right inRator
  -- Unary Let
  unparse (New _ t _) = (newRator, "let " ++ show t)
  -- Session expressions
  unparse (UnLet _ x e1 e2) = (inRator, "let " ++ show x ++ " = " ++ l ++ " in " ++ r)
    where l = bracket (unparse e1) Left inRator
          r = bracket (unparse e2) Right inRator
  unparse (Select _ l) = (appRator, "select " ++ show l) -- which rator?
  unparse (Match _ e m) = (inRator, "match " ++ s ++ " with {" ++ showFieldMap m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator

showFieldMap :: FieldMap -> String
showFieldMap m = intercalate "; " $ map showAssoc (Map.toList m)
 where
  showAssoc (b, (a, v)) =
    show b
      ++ " "
      ++ unwords (map show a)
      ++ " -> "
      ++ show v

{-
instance Show Expression where
  show = showExp 4
--  show = showExp 44

showExp :: Int -> Expression -> String
  -- Basic values
showExp _ (Unit _       ) = "()"
showExp _ (Integer   _ i) = show i
showExp _ (Character _ c) = show c
showExp _ (Boolean   _ b) = show b
  -- Variable
showExp _ (ProgVar   _ x) = show x
  -- Depth reached
showExp 0 _               = ".."
  -- Abstraction intro and elim
showExp i (Abs _ m b e) =
  "(λ"
    ++ show b
    ++ showArrow m
    ++ showExp (i - 1) e
    ++ ")"
showExp i (App _ e1 e2) =
  "(" ++ showExp (i - 1) e1 ++ " " ++ showExp (i - 1) e2 ++ ")"
  -- Pair intro and elim
showExp i (Pair _ e1 e2) =
  "(" ++ showExp (i - 1) e1 ++ ", " ++ showExp (i - 1) e2 ++ ")"
showExp i (BinLet _ x y e1 e2) =
  "(let ("
    ++ show x
    ++ ", "
    ++ show y
    ++ ") = "
    ++ showExp (i - 1) e1
    ++ " in "
    ++ showExp (i - 1) e2
    ++ ")"
  -- Datatype elim
showExp i (Case _ e m) =
  "case " ++ showExp (i - 1) e ++ " of {" ++ showFieldMap (i - 1) m ++ "}"
  -- Type Abstraction intro and elim
showExp _ (TypeApp _ x ts) =
  show x ++ " [" ++ unwords (map show ts) ++ "]"
showExp i (TypeAbs _ b e) =
  show "Λ" ++ show b ++ "->" ++ showExp (i - 1) e
  -- Boolean elim
showExp i (Conditional _ e e1 e2) =
  "if "
    ++ show e
    ++ " then "
    ++ showExp (i - 1) e1
    ++ " else "
    ++ showExp (i - 1) e2
  -- Let
showExp i (UnLet _ x e1 e2) =
  "(let "
    ++ show x
    ++ " = "
    ++ showExp (i - 1) e1
    ++ " in "
    ++ showExp (i - 1) e2
    ++ ")"
  -- Session types
showExp _ (New _ t _) = "new " ++ show t
showExp i (Select _ l) = "(select " ++ show l ++ ")"
showExp i (Match _ e m) =
  "match " ++ showExp (i - 1) e ++ " with {" ++ showFieldMap (i - 1) m ++ "}"

showFieldMap :: Int -> FieldMap -> String
showFieldMap i m = intercalate "; " (map showAssoc (Map.toList m))
 where
  showAssoc (b, (a, v)) =
    show b
      ++ " "
      ++ unwords (map show a)
      ++ " -> "
      ++ showExp (i - 1) v
-}

-- Type Schemes

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = intercalate ", " (map show bs)
