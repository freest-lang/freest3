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
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable            ( TypeVar )

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (K.Kind, T.Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar T.Type

-- The names of the functions from the Prelude
type PreludeNames = [ProgVar]

-- Mapping between positions & type operators (Typename & Dualof)
-- Used to give better error messages

type TypeOpsEnv = Map.Map Pos T.Type

-- The definitions of the named functions in a program
type Prog = Map.Map ProgVar E.Exp


-- A given type environment without constructors
noConstructors :: TypeEnv -> VarEnv -> VarEnv
noConstructors tEnv =
  Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tEnv))


-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: ProgVar -> TypeEnv -> Bool
isDatatypeContructor c tEnv = not $ Map.null $ Map.filter (isDatatype . snd)
                                                          tEnv
 where
  isDatatype :: T.Type -> Bool
  isDatatype (T.Rec _ (K.Bind _ _ _ t)) =  isDatatype t
--  isDatatype (T.Forall _ (K.Bind _ _ _ t)) =  isDatatype t
  isDatatype (T.App _ t u) =  isDatatype t || isDatatype u
  isDatatype (T.Abs _ (K.Bind _ _ _ t)) =  isDatatype t
  isDatatype (T.Variant _ m) = c `Map.member` m
  isDatatype _                = False
