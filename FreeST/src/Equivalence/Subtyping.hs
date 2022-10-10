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

{-# LANGUAGE FlexibleInstances #-}

module Equivalence.Subtyping
  ( Subtyping(..)
  )
where

import           Bisimulation.Bisimulation        ((<~))
import           Syntax.Program                   ( VarEnv )
import qualified Syntax.Type               as T   
import qualified Data.Map.Strict           as Map 


class Subtyping t where
  (<:) :: t -> t -> Bool

instance Subtyping T.Type where
  (<:) = (<~)
  
instance Subtyping VarEnv where
  env1 <: env2 =
    Map.size env1
      == Map.size env2
      && Map.foldlWithKey
           (\acc b s ->
             acc && b `Map.member` env2 && (s <: (env2 Map.! b))
           )
           True
           env1
 
