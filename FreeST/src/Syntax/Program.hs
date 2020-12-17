module Syntax.Program
  ( TypeEnv
  , VarEnv
  , TypeOpsEnv
  , noConstructors
  )
where


import           Syntax.Base
import qualified Syntax.Type as T
import           Syntax.TypeVariable            ( TypeVar )
import           Syntax.ProgramVariable         ( ProgVar )
import qualified Syntax.Kind                   as K
import qualified Data.Map.Strict               as Map

-- The definitions of the datatypes and types declared in a program
type TypeEnv = Map.Map TypeVar (K.Kind, T.Type)

-- The signatures of the functions names (including the primitive
-- operators) and parameters, and the datatype constructors
type VarEnv = Map.Map ProgVar T.Type

-- POSITIONS & TYPE OPERATORS (TYPENAME & DUALOF)

type TypeOpsEnv = Map.Map Pos T.Type


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
  isDatatype (T.Datatype _ m) = c `Map.member` m
  isDatatype _              = False
