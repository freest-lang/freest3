{-# LANGUAGE FlexibleInstances #-}
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

import           Data.Char                      ( isDigit )
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( Left
                                                , Right
                                                ) -- needed for Associativity
import           Syntax.Base
import           Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable

-- Positions (Base)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c

-- Multiplicities (Base)

instance Show Multiplicity where
  show Un  = "U"
  show Lin = "L"

showArrow :: Multiplicity -> String
showArrow Lin = "-o"
showArrow Un  = "->"

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

-- Sorted variable. Either a:k or x:t (just to get the spacing right)

showSortedVar :: (Show a, Show b) => a -> b -> String
showSortedVar x t = show x ++ ":" ++ show t

-- Kind

instance Show K.Basic where
  show K.Session = "S"
  show K.Message = "M"
  show K.Top     = "T"

instance Show K.Kind where
  show (K.Kind _ p m) = show p ++ show m

-- Binds

showKind :: (Show a, Show b, Show c) => a -> b -> String -> c -> String
showKind var sort arrow term =
  showSortedVar var sort ++ " " ++ arrow ++ " " ++ show term

instance Show t => Show (K.Bind t) where
  show (K.Bind _ a k t) = showKind a k "=>" t

-- Type bind

instance Show E.Bind where
  show (E.Bind _ m x t e) = showKind x t (showArrow m) e

-- Polarity

instance Show T.Polarity where
  show T.In  = "?"
  show T.Out = "!"

showChoiceView :: T.Polarity -> String
showChoiceView T.In  = "&"
showChoiceView T.Out = "+"

-- Unparsing types and expressions

-- Norman Ramsey, Unparsing Expressions With Prefix and Postfix
-- Operators, Software—Practice and Experience, 1998.
-- https://www.cs.tufts.edu/~nr/pubs/unparse.ps

data Precedence =
  PMin |
  PIn | PNew | PDot | PArrow | PSemi | PDualof | PMsg | PApp |
  PMax
  deriving (Eq, Ord, Bounded)

data Associativity = Left | Right | NonAssoc deriving Eq

type Rator = (Precedence, Associativity)

type Fragment = (Rator, String)

inRator, newRator, dotRator, arrowRator, semiRator,
 dualofRator, appRator, minRator, maxRator, msgRator 
  :: Rator
inRator = (PIn, Right)       -- also else, match, case
newRator = (PNew, NonAssoc)
dotRator = (PDot, Right)
arrowRator = (PArrow, Right)
semiRator = (PSemi, Right)
msgRator = (PMsg, Right)
dualofRator = (PDualof, Right)
appRator = (PApp, Left)
minRator = (minBound, NonAssoc)
maxRator = (maxBound, NonAssoc)

noparens :: Rator -> Rator -> Associativity -> Bool
noparens (pi, ai) (po, ao) side = pi > po || pi == po && ai == ao && ao == side

bracket :: Fragment -> Associativity -> Rator -> String
bracket (inner, image) side outer | noparens inner outer side = image
                                  | otherwise = "(" ++ image ++ ")"

class Unparse t where
  unparse :: t -> Fragment

-- Type

instance Show T.Type where
  show = snd . unparse

instance Unparse T.Type where
  unparse (T.Int  _       ) = (maxRator, "Int")
  unparse (T.Char _       ) = (maxRator, "Char")
  unparse (T.Bool _       ) = (maxRator, "Bool")
  unparse (T.Unit _       ) = (maxRator, "()")
  unparse (T.Skip _       ) = (maxRator, "Skip")
  unparse (T.Var  _ a     ) = (maxRator, show a)
  unparse (T.Message _ p t) = (msgRator, show p ++ m)
    where m = bracket (unparse t) Right msgRator
  unparse (T.Fun _ m t u) = (arrowRator, l ++ showArrow m ++ r)
   where
    l = bracket (unparse t) Left arrowRator
    r = bracket (unparse u) Right arrowRator
  unparse (T.Pair _ t u) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
   where
    l = bracket (unparse t) Left minRator
    r = bracket (unparse u) Right minRator
  unparse (T.Datatype _ m) = (maxRator, "[" ++ showDatatype m ++ "]")
  unparse (T.Semi _ t u  ) = (semiRator, l ++ " ; " ++ r)
   where
    l = bracket (unparse t) Left semiRator
    r = bracket (unparse u) Right semiRator
  unparse (T.Choice _ v m) =
    (maxRator, showChoiceView v ++ "{" ++ showChoice m ++ "}")
  unparse (T.Forall _ b) = (dotRator, "∀" ++ show b) -- ++ "=>" ++ s)
    -- where s = bracket (unparse t) Right dotRator
  unparse (T.Rec _ b) = (dotRator, "rec " ++ show b) -- xk ++ "." ++ s)
    -- where s = bracket (unparse t) Right dotRator
  unparse (T.Dualof _ t) = (dualofRator, "dualof " ++ s)
    where s = bracket (unparse t) Right dualofRator

showDatatype :: T.TypeMap -> String
showDatatype m = intercalate " | "
  $ Map.foldrWithKey (\c t acc -> (show c ++ showAsSequence t) : acc) [] m
 where
  showAsSequence :: T.Type -> String
  showAsSequence (T.Fun _ _ t u) = " " ++ show t ++ showAsSequence u
  showAsSequence _               = ""

showChoice :: T.TypeMap -> String
showChoice m = intercalate ", "
  $ Map.foldrWithKey (\c t acc -> (show c ++ ": " ++ show t) : acc) [] m

-- Expression

instance Show Exp where
  show = snd . unparse

instance Unparse Exp where
  -- Basic values
  unparse (E.Unit _  ) = (maxRator, "()")
  unparse (E.Int  _ i) = (maxRator, show i)
  unparse (E.Char _ c) = (maxRator, show c)
  unparse (E.Bool _ b) = (maxRator, show b)
  -- Variable
  unparse (E.Var  _ x) = (maxRator, show x)
  -- Abstraction intro and elim
  unparse (E.Abs _ b) = (arrowRator, "λ" ++ show b) -- ++ showArrow m ++ s)
    -- where s = bracket (unparse e) Right arrowRator
  unparse (E.App _ e1 e2) = (appRator, l ++ " " ++ r)
   where
    l = bracket (unparse e1) Left appRator
    r = bracket (unparse e2) Right appRator
  -- Pair intro and elim
  unparse (E.Pair _ e1 e2) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
   where
    l = bracket (unparse e1) Left minRator
    r = bracket (unparse e2) Right minRator
  unparse (E.BinLet _ x y e1 e2) =
    (inRator, "let " ++ p ++ " = " ++ l ++ " in " ++ r)
   where
    p = "(" ++ show x ++ ", " ++ show y ++ ")"
    l = bracket (unparse e1) Left inRator
    r = bracket (unparse e2) Right inRator
  -- Datatype elim
  unparse (E.Case _ e m) =
    (inRator, "case " ++ s ++ " of {" ++ showFieldMap m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator
  -- Type Abstraction intro and elim
  unparse (E.TypeApp _ x t) = (appRator, show x ++ " [" ++ show t ++ "]")
  unparse (E.TypeAbs _ b) = (arrowRator, "λ" ++ show b) -- ++ "->" ++ s)
    -- where s = bracket (unparse e) Right arrowRator
  -- Boolean elim
  unparse (E.Cond _ e1 e2 e3) =
    (inRator, "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3)
   where
    s1 = bracket (unparse e1) Left inRator
    s2 = bracket (unparse e2) NonAssoc inRator
    s3 = bracket (unparse e3) Right inRator
  -- Unary Let
  unparse (E.New _ t _) = (newRator, "let " ++ show t)
  -- Session expressions
  unparse (E.UnLet _ x e1 e2) =
    (inRator, "let " ++ show x ++ " = " ++ l ++ " in " ++ r)
   where
    l = bracket (unparse e1) Left inRator
    r = bracket (unparse e2) Right inRator
  unparse (E.Select _ l) = (appRator, "select " ++ show l) -- which rator?
  unparse (E.Match _ e m) =
    (inRator, "match " ++ s ++ " with {" ++ showFieldMap m ++ "}")
    where s = bracket (unparse e) NonAssoc inRator

showFieldMap :: FieldMap -> String
showFieldMap m = intercalate "; " $ map showAssoc (Map.toList m)
 where
  showAssoc (b, (a, v)) =
    show b ++ " " ++ unwords (map show a) ++ " -> " ++ show v

-- VarEnv

instance {-# OVERLAPPING #-} Show VarEnv where
  show venv = "[" ++ intercalate "\n\t\t   ," (venvToList venv) ++ "]"

venvToList :: VarEnv -> [String]
venvToList =
  Map.foldrWithKey (\k v acc -> showSortedVar k v : acc) []
