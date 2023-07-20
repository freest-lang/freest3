module Syntax.Program
  ( TypeOpsEnv
  , noConstructors
  , isDatatypeContructor
  , isDatatype
  )
where


import qualified Data.Map.Strict as Map
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Type as T

-- Mapping between positions & type operators (Typename & Dualof)
-- Used to give better error messages
-- TODO: remove upon merge to keep-source
type TypeOpsEnv = Map.Map Span T.Type

-- A given type environment without constructors
noConstructors :: Types -> Signatures -> Signatures
noConstructors tys = Map.filterWithKey (\x _ -> not (x `isDatatypeContructor` tys))

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type Environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: Variable -> Types -> Bool
isDatatypeContructor c = not . Map.null . Map.filter (isInDatatype . snd)
 where
  isInDatatype :: T.Type -> Bool
  isInDatatype (T.Rec _ (Bind _ _ _ t)) =  isInDatatype t
  isInDatatype (T.Labelled _ T.Variant m) = c `Map.member` m
  isInDatatype _                = False

isDatatype :: T.Type -> Bool
isDatatype (T.Rec _ (Bind _ _ _ t)) =  isDatatype t
isDatatype (T.Labelled _ T.Variant _) = True 
isDatatype _                = False
