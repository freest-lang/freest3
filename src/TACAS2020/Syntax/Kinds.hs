{- |
Module      :  Syntax.Kinds
Description :  The kind of a type
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module defines a kind and the relational order between kinds. It also defines the
subkinding relation, the least upper bound of two kinds and other functions to
manipulate prekinds and multiplicities.
-}

module Syntax.Kinds
( PreKind (..)
, TypeVarBind(..)
, Kind (..)
, KindEnv
, kindTL
, kindTU
, kindSL
, kindSU
, isSession
, (<:)
, join
, isLin
, isUn
, fromTypeVarBinds
) where

import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

-- Prekinds

data PreKind = Session | Functional deriving Eq

instance Ord PreKind where
   Session <= Functional = True
   _       <= _          = False

-- Kinds

data Kind = Kind Pos PreKind Multiplicity deriving Ord

instance Eq Kind where
  (Kind _ p n) == (Kind _ q m) = (p, n) == (q, m)

-- The subkinding relation. Note that Kind is a partial order, hence
-- should not implement class Ord.
--    TL
--   /  \
-- TU    SL
--   \  /
--    SU
(<:) :: Kind -> Kind -> Bool
(Kind _ Session Un)    <: _                       = True
(Kind _ Session Lin)   <: (Kind _ Functional Lin) = True
(Kind _ Functional Un) <: (Kind _ Functional Lin) = True
k1                     <: k2                      = k1 == k2

-- The least upper bound of two kinds
join :: Kind -> Kind -> Kind
join (Kind p Functional Un)  (Kind _ Session    Lin) = omission p
join (Kind p Session    Lin) (Kind _ Functional Un)  = omission p
join k1                      k2                      = if k1 <: k2 then k2 else k1

-- The four kinds
kindTL p = Kind p Functional Lin
kindTU p = Kind p Functional Un
kindSL p = Kind p Session Lin
kindSU p = Kind p Session Un

-- The kind that sits at the top of the hierarchy (use as a default value)
instance Default Kind where
  omission = kindTL

isSession :: Kind -> Bool
isSession = (<: (Kind defaultPos Session Lin))

isLin :: Kind -> Bool
isLin (Kind _ _ m) = m == Lin

isUn :: Kind -> Bool
isUn = not . isLin

instance Position Kind where
  position (Kind p _ _) = p

-- Kind environments

type KindEnv = Map.Map TypeVar Kind

-- Binding type variables to kinds

data TypeVarBind = TypeVarBind Pos TypeVar Kind deriving (Eq, Ord)

instance Position TypeVarBind where
  position (TypeVarBind p _ _) = p

fromTypeVarBinds :: [TypeVarBind] -> KindEnv
fromTypeVarBinds = foldr (\(TypeVarBind _ x k) env -> Map.insert x k env) Map.empty
