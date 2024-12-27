{- |
Module      :  Syntax.Base
Description :  The Base module.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines the basic structures (classes and datatypes) such as Positions and
Multiplicity, that will be used the remaining Compiler.
-}

module Syntax.Base
  ( Default(..)
  , Pos
  , Multiplicity(..)
  , Bind(..)
  , Variable(..)
  , Span(..)
  , Located(..)
  , defaultSpan
  , intern
  , extern
  , mkVar
  , mkNewVar
  , isWild
) where

-- Default value for the various syntactic categories

class Default t where
  omission :: Span -> t

-- Position: line and column

type Pos = (Int, Int)

-- Span: the name of the module plus start and end position

data Span = Span
  { moduleName :: FilePath
  , startPos   :: Pos
  , endPos     :: Pos
  } deriving (Eq, Ord)

defaultSpan :: Span
defaultSpan = Span "<default>" (0, 0) (0, 0)

-- The span of the various syntactic categories

class Located t where
  getSpan :: t -> Span

-- Multiplicity for types and expressions

data Multiplicity = Un | Lin | MultVar Variable deriving (Ord, Eq)

-- Variables for types and expressions

data Variable = Variable Span String Int

instance Eq Variable where
  x == y = intern x == intern y
 
instance Ord Variable where
  x <= y = intern x <= intern y

instance Located Variable where
  getSpan (Variable s _ _) = s

instance Default Variable where
  omission s = mkVar s "<default>"

-- The internal representation of a variable
intern :: Variable -> String
intern (Variable _ str (-1)) = str -- We need this because renaming comes too late; renameProgram should be moved to a position after parse
intern (Variable _ _ n) = show n

-- The program-level representation of a variable
extern :: Variable -> String
extern (Variable _ str _) = str
-- extern = intern -- for debugging purposes

-- Make a variable from a span and string; the internal representation is default (-1)
mkVar :: Span -> String -> Variable
mkVar s str = Variable s str (-1)

-- Make a variable from an integer and a variable. The new variable is
-- unique if the integer is.
mkNewVar :: Int -> Variable -> Variable
mkNewVar next (Variable s str _) = Variable s str next

isWild :: Variable -> Bool
isWild (Variable _ str _) = str == "_"

-- Bind for (λ x:t -> e), (∀ a:k . t) or (Λ a:k => e)
data Bind a b = Bind {bSpan :: Span, var :: Variable, binder :: a, body :: b}
