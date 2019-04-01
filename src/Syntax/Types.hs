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

module Syntax.Types
( Dual(..)
, TBindK(..)
, BasicType(..)
, TypeMap(..)
, Polarity(..)
, Type(..)
, TypeScheme(..)
-- , free
, subs
, unfold
, toList -- TODO: not quite sure this belongs here
) where

import           Parse.Lexer (Position, Pos, position)
import           Syntax.Bind
import           Syntax.Kinds
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- DUALITY

class Dual t where
  dual :: t -> t

-- POLARITY

data Polarity =
    In
  | Out
  deriving (Eq, Ord)

instance Show Polarity where
  show In  = "?"
  show Out = "!"

instance Dual Polarity where
  dual In  = Out
  dual Out = In

-- BASIC TYPES

data BasicType =
    IntType
  | CharType
  | BoolType
  | UnitType
  deriving (Eq, Ord)

instance Show BasicType where
  show IntType  = "Int"
  show CharType = "Char"
  show BoolType = "Bool"
  show UnitType = "()"

-- TYPES

data Type =
  -- Functional types
    Basic Pos BasicType
  | Fun Pos Multiplicity Type Type
  | PairType Pos Type Type
  | Datatype Pos TypeMap
  -- Session types
  | Skip Pos
  | Semi Pos Type Type
  | Message Pos Polarity BasicType
  | Choice Pos Polarity TypeMap
  | Rec Pos TBindK Type
  -- Functional or session
  | Var Pos TVar -- a recursion variable if bound, polymorphic otherwise
  -- Type operators
  | Name Pos TVar   -- a named type, to be looked upon in a map of Cons to Type
  | Dualof Pos Type -- to be expanded into a session type
  deriving Ord

type TypeMap = Map.Map PBind Type -- TODO: rename to FieldMap

instance Eq Type where -- Type equality, up to alpha-conversion
  t == u = equalTypes Map.empty t u 

equalTypes :: Map.Map TVar TVar -> Type -> Type -> Bool
  -- Functional types
equalTypes s (Basic _ x)      (Basic _ y)      = x == y
equalTypes s (Fun _ m t u)    (Fun _ n v w)    = m == n && equalTypes s t v && equalTypes s u w
equalTypes s (PairType _ t u) (PairType _ v w) = equalTypes s t v && equalTypes s u w
equalTypes s (Datatype _ m1)  (Datatype _ m2)  = equalMaps s m1 m2
  -- Session types
equalTypes s (Skip _)         (Skip _)         = True
equalTypes s (Semi _ t1 t2)   (Semi _ u1 u2)   = equalTypes s t1 u1 && equalTypes s t2 u2
equalTypes s (Message _ p x)  (Message _ q y)  = p == q && x == y
equalTypes s (Choice _ v1 m1) (Choice _ v2 m2) = v1 == v2 && equalMaps s m1 m2
equalTypes s (Rec _ (TBindK _ x _) t) (Rec _ (TBindK _ y _) u) = equalTypes (Map.insert x y s) t u
  -- Functional or session
equalTypes s (Var _ x)        (Var _ y)        = equalVars (Map.lookup x s) x y
  -- Type operators
equalTypes s (Dualof _ t)     (Dualof _ u)     = t == u
equalTypes s (Name _ x)       (Name _ y)       = x == y
  -- Otherwise
equalTypes _ _              _                  = False

equalVars :: Maybe TVar -> TVar -> TVar -> Bool
equalVars Nothing  y z = y == z
equalVars (Just x) _ z = x == z

equalMaps :: Map.Map TVar TVar -> TypeMap -> TypeMap -> Bool
equalMaps s m1 m2 =
  Map.size m1 == Map.size m2 &&
    Map.foldlWithKey(\b l t ->
      b && l `Map.member` m2 && equalTypes s t (m2 Map.! l)) True m1

instance Show Type where
  -- Functional types
  show (Basic _ b)      = show b
  show (Fun _ m t u)    = "(" ++ show t ++ showFunOp m ++ show u ++ ")"
  show (PairType _ t u) = "(" ++ show t ++ ", " ++ show u ++ ")"
  show (Datatype _ m)   = "["++ showMap m ++"]"
  -- Session types
  show (Skip _)         = "Skip"
  show (Semi _ t u)     = "(" ++ show t ++ ";" ++ show u ++ ")"
  show (Message _ p b)  = show p ++ show b
  show (Choice _ v m)   = show v ++ "{" ++ showMap m ++ "}"
  show (Rec _ x t)      = "(rec " ++ show x ++ " . " ++ show t ++ ")"
  -- Functional or session
  show (Var _ x)        = x
  -- Type operators
  show (Dualof _ s)     = "(dualof " ++ show s ++ ")"
  show (Name _ x)       = x
  
showFunOp :: Multiplicity -> String
showFunOp Lin = " -o "
showFunOp Un  = " -> "

showChoice :: Polarity -> String
showChoice In  = "&"
showChoice Out = "+"

showMap :: TypeMap -> String
showMap m = concat $ intersperse ", " (map showAssoc (Map.assocs m))
  where showAssoc (b, v) = show b ++ ": " ++ show v

instance Position Type where
  -- Functional types
  position (Basic p _)     = p
  position (Fun p _ _ _)    = p
  position (PairType p _ _) = p
  position (Datatype p _)   = p
  -- Session types
  position (Skip p)         = p
  position (Semi p _ _)     = p
  position (Message p _ _)  = p
  position (Choice p _ _)   = p
  position (Rec p _ _)      = p
  -- Functional or session
  position (Var p _)        = p
  -- Type operators
  position (Dualof p _)     = p
  position (Name p _)       = p

instance Dual Type where
  -- Session types
  dual (Semi p t1 t2)  = Semi p (dual t1) (dual t2)
  dual (Message p v b) = Message p (dual v) b
--  dual (Choice p v m)  = Choice p (dual v) (Map.map dual m)
  dual (Choice p v m)  = Choice p (dual v) (Map.map (Dualof p) m) -- The lazy version, hopefully faster
--  dual (Rec p x t)     = Rec p x (dual t)
  dual (Rec p x t)     = Rec p x (Dualof p t) -- The lazy version, hopefully faster
  -- Type operators
  dual (Dualof _ t)    = t
  -- Functional types, Skip, Var, Name
  dual t               = t

-- KINDED TYPE BIND

data TBindK = TBindK Pos TVar Kind

instance Eq TBindK where
  (TBindK _ x _) == (TBindK _ y _) = x == y

instance Ord TBindK where
  (TBindK _ x _) `compare` (TBindK _ y _) = x `compare` y

instance Show TBindK where
  show (TBindK _ x k) = x ++ " : " ++ show k

instance Position TBindK where
  position (TBindK p _ _) = p

-- TYPE SCHEMES

data TypeScheme = TypeScheme Pos [TBindK] Type

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

toList :: TypeScheme -> [TypeScheme] -- TODO: return [Type]
toList (TypeScheme p b (Fun _ _ t1 t2)) = (TypeScheme p b t1) : toList (TypeScheme p b t2)
toList t = [t]

-- UNFOLDING, SUBSTITUTING

unfold :: Type -> Type
-- Assumes parameter is a Rec type
unfold (Rec p x t) = subs (Rec p x t) x t

-- [u/x]t, substitute u for x on t
subs :: Type -> TBindK -> Type -> Type 
  -- Functional types
subs t y (Fun p m t1 t2)    = Fun p m (subs t y t1) (subs t y t2)
subs t y (PairType p t1 t2) = PairType p (subs t y t1) (subs t y t2)
subs t y (Datatype p m)     = Datatype p (Map.map(subs t y) m)
  -- Session types
subs t y (Semi p t1 t2)     = Semi p (subs t y t1) (subs t y t2)
subs t y (Choice p v m)     = Choice p v (Map.map(subs t y) m)
subs t2 y (Rec p x t1)      -- Assume y /= x
  | x == y                  = Rec p x t1
  | otherwise               = Rec p x (subs t2 y t1)
  -- Functional or session
subs t (TBindK _ y _) (Var p x)
  | x == y                  = t
  | otherwise               = Var p x
  -- Type operators  
subs t y (Dualof p t1)      = Dualof p (subs t y t1)
  -- Otherwise: Basic, Skip, Message, Name
subs _ _ t                  = t
