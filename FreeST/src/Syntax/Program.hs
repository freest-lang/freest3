module Syntax.Program
  ( TypeEnv
  , VarEnv
  , Prog
  , TypeOpsEnv
  , PreludeNames
  , noConstructors
  , isDatatypeContructor
  , isDatatype
  )
where


import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import Syntax.AST

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map Variable (K.Kind, T.Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map Variable T.Type

-- The names of the functions from the Prelude
type PreludeNames = [Variable]

-- Mapping between positions & type operators (Typename & Dualof)
-- Used to give better error messages

type TypeOpsEnv = Map.Map Span T.Type

-- The definitions of the named functions in a program
type Prog = Map.Map Variable E.Exp


-- A given type environment without constructors
noConstructors :: Types -> Signatures -> Signatures
noConstructors tEnv =
  Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))

-- -- A given type environment without constructors
-- noConstructors :: TypeEnv -> VarEnv -> VarEnv
-- noConstructors tEnv =
--   Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))


-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: Variable -> Types -> Bool
isDatatypeContructor c tEnv = not $ Map.null $ Map.filter (isInDatatype . snd)
                                                          tEnv
 where
  isInDatatype :: T.Type -> Bool
  isInDatatype (T.Rec _ (Bind _ _ _ t)) =  isInDatatype t
  isInDatatype (T.Labelled _ T.Variant m) = c `Map.member` m
  isInDatatype _                = False

-- isDatatypeContructor :: Variable -> TypeEnv -> Bool
-- isDatatypeContructor c tEnv = not $ Map.null $ Map.filter (isInDatatype . snd)
--                                                           tEnv
--  where
--   isInDatatype :: T.Type -> Bool
--   isInDatatype (T.Rec _ (Bind _ _ _ t)) =  isInDatatype t
--   isInDatatype (T.Labelled _ T.Variant m) = c `Map.member` m
--   isInDatatype _                = False


isDatatype :: T.Type -> Bool
isDatatype (T.Rec _ (Bind _ _ _ t)) =  isDatatype t
isDatatype (T.Labelled _ T.Variant _) = True 
isDatatype _                = False
