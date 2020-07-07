module Utils.PreludeLoader
( prelude
, isBuiltin
, userDefined
, initialTEnv
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map
import Syntax.Kinds


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
  , (mkVar p "(/=)", relationalOp)
  , (mkVar p "(<)", relationalOp)
  , (mkVar p "(>)", relationalOp)
  , (mkVar p "(<=)", relationalOp)
  , (mkVar p "(>=)", relationalOp)
  
  , (mkVar p "ord",  fromType (Fun defaultPos Un (Basic defaultPos CharType) (Basic defaultPos IntType)))
  , (mkVar p "chr",  fromType (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos CharType)))
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", TypeScheme p [TypeVarBind p a (Kind p Functional Lin)] (Fun p Lin (TypeVar p a) (Basic p UnitType))) 
--           , (mkVar p "id", TypeScheme p [TBindK p "a" (Kind p Session Un)] (Fun p Un (TypeVar p "a") (TypeVar p "a")))
  , (mkVar p "printInt", fromType (Fun p Un (Basic p IntType) (Basic p UnitType)))
  , (mkVar p "printBool", fromType (Fun p Un (Basic p BoolType) (Basic p UnitType)))
  , (mkVar p "printChar", fromType (Fun p Un (Basic p CharType) (Basic p UnitType)))
  , (mkVar p "printUnit", fromType (Fun p Un (Basic p UnitType) (Basic p UnitType)))
--  , (mkVar p "print", TypeScheme p [varBind] (Fun p Un var (Basic p UnitType)))
  , (mkVar p "#Nil", fromType funList)
  , (mkVar p "#Cons", fromType funIntListList)
  ] 
  where p = defaultPos
        var = TypeVar p (mkVar p "a")
        varBind = TypeVarBind p (mkVar p "a") (omission p)

prelude :: VarEnv
prelude = foldl (\acc (x, s) -> Map.insert x s acc) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` (map fst typeList))

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))

initialTEnv :: TypeEnv
initialTEnv = 
  Map.fromList[(intList,(omission defaultPos,fromType dataType))]
  where dataType = Datatype defaultPos 
                   (Map.fromList[(mkVar defaultPos ("#Cons"), funIntListList),
                                 (mkVar defaultPos ("#Nil"), funList)])

-- TODO(J) Make this pretty :)
intList = mkVar defaultPos ("#IntList")
funList = TypeName defaultPos intList
funIntListList = Fun defaultPos Un (Basic defaultPos IntType) 
        (Fun defaultPos Un (TypeName defaultPos intList) (TypeName defaultPos intList))