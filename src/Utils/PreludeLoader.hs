module Utils.PreludeLoader
( prelude
, isBuiltin
, userDefined
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

binIntOp = fromType (Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType)))
binBoolOp = fromType (Fun defaultPos Un (Basic defaultPos BoolType) (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType)))
relationalOp = fromType(Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType)))
unIntBool = fromType (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))
unIntInt = fromType (Fun defaultPos Un (Basic defaultPos IntType)  (Basic defaultPos IntType))
unBoolBool = fromType (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))

typeList :: [(ProgVar, TypeScheme)]
typeList =
  [ (mkVar p "(+)", binIntOp)
  , (mkVar p "(-)", binIntOp)
  , (mkVar p "(/)", binIntOp)
  , (mkVar p "(*)", binIntOp)
  , (mkVar p "mod", binIntOp)
  , (mkVar p "rem", binIntOp)
  , (mkVar p "div", binIntOp)
  , (mkVar p "negate", unIntInt)
  , (mkVar p "not", unBoolBool)
  , (mkVar p "(&&)", binBoolOp)
  , (mkVar p "(||)", binBoolOp)
  , (mkVar p "(==)", relationalOp)
  , (mkVar p "(<)", relationalOp)
  , (mkVar p "(>)", relationalOp)
  , (mkVar p "(<=)", relationalOp)
  , (mkVar p "(>=)", relationalOp)
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", TypeScheme p [TypeVarBind p a (Kind p Functional Lin)] (Fun p Lin (TypeVar p a) (Basic p UnitType))) 
--           , (mkVar p "id", TypeScheme p [TBindK p "a" (Kind p Session Un)] (Fun p Un (TypeVar p "a") (TypeVar p "a")))
  ]
  where p = defaultPos

prelude :: VarEnv
prelude = foldl (\acc (x, s) -> Map.insert x s acc) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` (map fst typeList))

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))
