{-|
Module      :  Kinding.Contractive
Description :  Is a given type contractive on a recursion variable,
               given a set of polymorphic variables?
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Rules from

Bernardo Almeida, Andreia Mordido, Peter Thiemann, Vasco T. Vasconcelos:
Polymorphic lambda calculus with context-free session types.
Inf. Comput. 289(Part): 104948 (2022)

except that we omit rule C-TAbs given that forall does exhibit a type
constructor. We want type (rec a . forall b: 1S . a) to be well formed.
-}

module Kinding.Contractive
  ( contractive
  )
where

import           Syntax.Base            (Bind(..), Variable)
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Kinding.Terminated
import qualified Data.Set as Set

contractive :: K.PolyVars -> Variable -> T.Type -> Bool
contractive s a (T.Semi _ t u)
  | terminated t                         = contractive s a u             -- C-Seq1
  | otherwise                            = contractive s a t             -- C-Seq2
contractive s a (T.Rec _ (Bind _ _ _ t)) = contractive s a t             -- C-Rec
contractive s a (T.Var _ b)              = b /= a && b `Set.notMember` s -- C-Var
contractive _ _ _                        = True                          -- C-Other
