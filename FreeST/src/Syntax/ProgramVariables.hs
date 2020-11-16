{- |
Module      :  Syntax.ProgramVariables
Description :  The program variables
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

The definition of program variables
-}

module Syntax.ProgramVariables
( ProgVar
  , isADT -- TODO: remove
  , ProgVarTuple(..)                                                            -- CHANGED
) where

import Syntax.Base
import Data.Char (isUpper) -- TODO: remove

-- Note: isomorphic to TypeVariables: Type <-> Prog
data ProgVar = ProgVar Pos String

instance Variable ProgVar where
  mkVar = ProgVar
  mkNewVar next (ProgVar pos id) = ProgVar pos (show next ++ '#' : id)
  intern (ProgVar _ x) = x

instance Eq ProgVar where
  (ProgVar _ x) == (ProgVar _ y) = x == y

instance Ord ProgVar where
  (ProgVar _ x) <= (ProgVar _ y) = x <= y

instance Position ProgVar where
  position (ProgVar p _) = p

-- TODO: remove
isADT :: ProgVar -> Bool
isADT (ProgVar _ (x:_)) = isUpper x


data ProgVarTuple = Tuple Pos ProgVar ProgVarTuple | Single Pos ProgVar         -- CHANGED

instance Eq ProgVarTuple where                                                  -- CHANGED
  (Tuple _ pv1 pvt1) == (Tuple _ pv2 pvt2) = pv1 == pv2 && pvt1 == pvt2
  (Single _ pv1) == (Single _ pv2) = pv1 == pv2
  _ == _ = False
