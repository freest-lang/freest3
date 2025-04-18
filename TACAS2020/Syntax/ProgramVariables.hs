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
) where

import Syntax.Base

-- Note: isomorphic to TypeVariables: Type <-> Prog
data ProgVar = ProgVar Pos String

instance Variable ProgVar where
  mkVar = ProgVar
  mkNewVar next (ProgVar pos id) = ProgVar pos (show next ++ '_' : id)
  intern (ProgVar _ x) = x

instance Eq ProgVar where
  (ProgVar _ x) == (ProgVar _ y) = x == y
  
instance Ord ProgVar where
  (ProgVar _ x) <= (ProgVar _ y) = x <= y

instance Position ProgVar where
  position (ProgVar p _) = p
