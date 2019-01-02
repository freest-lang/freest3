{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Grammar
( Label(..)
, Grammar
, GNF(..)
, transitions
) where

import qualified Data.Map.Strict as Map
import           Syntax.Types


-- TYPE GRAMMAR

data Label =
  ChoiceLabel ChoiceView Constructor |
  MessageLabel Polarity BasicType |
  VarLabel TypeVar
  deriving (Eq, Ord)

instance Show Label where
  show (ChoiceLabel v l) = show v ++ l
  show (MessageLabel p t) = show p ++ show t
  show (VarLabel l) = l

type Grammar = Map.Map TypeVar (Map.Map Label [TypeVar])

data GNF = GNF {start :: TypeVar, productions :: Grammar}

-- Operations on grammars

transitions :: Grammar -> [TypeVar] -> Map.Map Label [TypeVar]
transitions _ []     = Map.empty
transitions g (x:xs) = Map.map (++ xs) (g Map.! x)

-- Show Grammar

instance Show GNF where
  show g = "start:" ++ start g ++ showGrammar (productions g)

showGrammar :: Grammar -> String
showGrammar = Map.foldrWithKey showProds ""

showProds :: TypeVar -> (Map.Map Label [TypeVar]) -> String -> String
showProds x m s = s ++ "\n" ++ Map.foldrWithKey (showProd x) "" m

showProd :: TypeVar -> Label -> [TypeVar] -> String -> String
showProd x l ys s = s ++ "\n" ++ x ++ " ::= " ++ show l ++ " " ++ (if null ys then "ε" else concat ys)
