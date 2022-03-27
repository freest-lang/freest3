module Syntax.Program
  ( TypeEnv
  , VarEnv
  , Prog
  , TypeOpsEnv
  , PreludeNames
  , noConstructors
  , isDatatypeContructor
  )
where


import qualified Data.Map.Strict               as Map
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map Variable (K.Kind, T.Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map Variable T.Type

-- The names of the functions from the Prelude
type PreludeNames = [Variable]

-- Mapping between positions & type operators (Typename & Dualof)
-- Used to give better error messages

type TypeOpsEnv = Map.Map Pos T.Type

-- The definitions of the named functions in a program
type Prog = Map.Map Variable E.Exp


-- A given type environment without constructors
noConstructors :: TypeEnv -> VarEnv -> VarEnv
noConstructors tEnv =
  Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))


-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: Variable -> TypeEnv -> Bool
isDatatypeContructor c tEnv = not $ Map.null $ Map.filter (isDatatype . snd)
                                                          tEnv
 where
  isDatatype :: T.Type -> Bool
  isDatatype (T.Rec _ (Bind _ _ _ t)) =  isDatatype t
  isDatatype (T.Variant _ m) = c `Map.member` m
  isDatatype _                = False
