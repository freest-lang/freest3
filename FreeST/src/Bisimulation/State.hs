{- |
Module      :  Bisimulation.State
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

This module TODO
-}

{-# LANGUAGE ImportQualifiedPost, LambdaCase, BlockArguments, NamedFieldPuns #-}

module Bisimulation.State
  ( Norm(..)
  , NormMap
  , Bisimulation
  , BisimulationState (..)
  , Basis
  , Bpa (..)
  , Node (..)
  , BranchQueue (..)
  , Branch
  , addLog
  , updateNormMap
  , putBasis
  , modifyBasis
  , putVisitedPairs
  , modifyVisitedPairs
  )
where

import Syntax.Base (Variable)
import SimpleGrammar.Grammar

import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Sequence qualified as Seq
import Prelude hiding ( Word, log )

data Norm = Normed Int | Unnormed
  deriving (Eq, Ord)

instance Num Norm where
  Normed n + Normed m = Normed (n + m)
  _        + _        = Unnormed

  Normed n * Normed m = Normed (n * m)
  _        * _        = Unnormed

  Normed n - Normed m = Normed (n - m)
  _        - _        = Unnormed

  abs (Normed n) = Normed (abs n)
  abs _          = Unnormed

  signum (Normed n) = Normed (signum n)
  signum Unnormed   = Unnormed

  fromInteger = Normed . fromInteger

-- Map to store the norm of each nonTerminal
type NormMap = Map.Map Variable Norm

-- isBisimilar types
data Node = Node
  { pair   :: (Word, Word)                                                     
  , parent :: Maybe Node
  } 
  deriving (Show, Eq)

type Branch = Node

type BranchQueue = Seq.Seq Branch

type Basis = Map.Map (Variable, Variable) Bpa

data Bpa = Bpa1 Word | Bpa2 (Word, Word)

instance Show Bpa where
  show (Bpa1 n) = show n
  show (Bpa2 (n, m)) = show (n, m)

type Bisimulation = State BisimulationState

data BisimulationState = BisimulationState
  { basis :: Basis
  , visitedPairs :: Set.Set (Word, Word)
  , normMap :: NormMap
  , grammar :: Grammar
  , log :: [String]
    -- ver log do haskell
  }

-- Functions created for the Monad type
-- Function to replace all elements in TStateData

putVisitedPairs :: Set.Set (Word, Word) -> Bisimulation ()
putVisitedPairs = modifyVisitedPairs . const

modifyVisitedPairs :: (Set.Set (Word, Word) -> Set.Set (Word, Word)) -> Bisimulation ()
modifyVisitedPairs f = do 
  ps <- gets visitedPairs
  modify \s -> s{visitedPairs = f (visitedPairs s)}

putBasis :: Basis -> Bisimulation ()
putBasis = modifyBasis . const

modifyBasis :: (Basis -> Basis) -> Bisimulation ()
modifyBasis f = do 
  ps <- gets basis
  modify \s -> s{basis = f (basis s)}

updateNormMap :: NormMap -> Bisimulation ()
updateNormMap normMap = modify \s -> s{normMap}

-- Function to add a log message
addLog :: String -> Bisimulation ()
addLog msg = modify \s -> s {log = log s ++ [msg]}

