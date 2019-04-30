{- |
Module      :  Syntax.Schemes
Description :  The language types schemes.
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a type scheme.

It also defines:

 - The type environment (TypeEnv): Contains the definitions of the datatypes and types
 declared in a program

 - Variable envionment (VarEnv): Contains the signatures of the functions names (including
 the primitive operators) and parameters, and the datatype constructors
-}

module Syntax.Schemes
( TypeScheme(..)
, TypeEnv
, VarEnv
, toTypeScheme
, noConstructors
) where

import           Syntax.Types
import           Syntax.Kinds (Kind, TypeVarBind)
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

data TypeScheme = TypeScheme Pos [TypeVarBind] Type

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (Kind, TypeScheme)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar TypeScheme

-- Create a type scheme from a type
toTypeScheme :: Type -> TypeScheme
toTypeScheme t = TypeScheme (position t) [] t

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

instance Default TypeScheme where
 omission p = TypeScheme p [] (omission p)

-- A given type environment without constructors
noConstructors :: TypeEnv -> VarEnv -> VarEnv
noConstructors tEnv = Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: ProgVar -> TypeEnv -> Bool
isDatatypeContructor c tEnv =
  not $ Map.null $ Map.filter (\(_, (TypeScheme _ _ t)) -> isDatatype t) tEnv
  where isDatatype :: Type -> Bool
        isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False
