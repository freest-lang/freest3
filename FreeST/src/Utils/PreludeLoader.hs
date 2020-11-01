module Utils.PreludeLoader
  ( prelude
  , isBuiltin
  , userDefined
  )
where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict               as Map
import           Syntax.Kinds

binIntOp :: Type
binIntOp =
  (Fun
    defaultPos
    Un
    (Basic defaultPos IntType)
    (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType))
  )

binBoolOp :: Type
binBoolOp =
  (Fun
    defaultPos
    Un
    (Basic defaultPos BoolType)
    (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))
  )

relationalOp :: Type
relationalOp =
  (Fun
    defaultPos
    Un
    (Basic defaultPos IntType)
    (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))
  )

unIntBool :: Type
unIntBool =
  (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))

unIntInt :: Type
unIntInt =
  (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType))

unBoolBool :: Type
unBoolBool =
  (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))

typeList :: [(ProgVar, Type)]
typeList =
  [ (mkVar p "(+)"   , binIntOp)
  , (mkVar p "(-)"   , binIntOp)
  , (mkVar p "(/)"   , binIntOp)
  , (mkVar p "(*)"   , binIntOp)
  , (mkVar p "mod"   , binIntOp)
  , (mkVar p "rem"   , binIntOp)
  , (mkVar p "div"   , binIntOp)
  , (mkVar p "negate", unIntInt)
  , (mkVar p "not"   , unBoolBool)
  , (mkVar p "(&&)"  , binBoolOp)
  , (mkVar p "(||)"  , binBoolOp)
  , (mkVar p "(==)"  , relationalOp)
  , (mkVar p "(/=)"  , relationalOp)
  , (mkVar p "(<)"   , relationalOp)
  , (mkVar p "(>)"   , relationalOp)
  , (mkVar p "(<=)"  , relationalOp)
  , (mkVar p "(>=)"  , relationalOp)
  , ( mkVar p "ord"
    , (Fun defaultPos Un (Basic defaultPos CharType) (Basic defaultPos IntType))
    )
  , ( mkVar p "chr"
    , (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos CharType))
    )
  , ( mkVar p "fork"
    , (Fun p Un (Basic p UnitType) (Basic p UnitType))
    )
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", TypeScheme p [KindBind p a (Kind p Functional Lin)] (Fun p Lin (TypeVar p a) (Basic p UnitType))) 
--           , (mkVar p "id", TypeScheme p [TBindK p "a" (Kind p Session Un)] (Fun p Un (TypeVar p "a") (TypeVar p "a")))
  -- Prints
  , (mkVar p "printInt"   , (Fun p Un (Basic p IntType) (Basic p UnitType)))
  , (mkVar p "printIntLn" , (Fun p Un (Basic p IntType) (Basic p UnitType)))
  , (mkVar p "printBool"  , (Fun p Un (Basic p BoolType) (Basic p UnitType)))
  , (mkVar p "printBoolLn", (Fun p Un (Basic p BoolType) (Basic p UnitType)))
  , (mkVar p "printChar"  , (Fun p Un (Basic p CharType) (Basic p UnitType)))
  , (mkVar p "printCharLn", (Fun p Un (Basic p CharType) (Basic p UnitType)))
  , (mkVar p "printUnit"  , (Fun p Un (Basic p UnitType) (Basic p UnitType)))
  , (mkVar p "printUnitLn", (Fun p Un (Basic p UnitType) (Basic p UnitType)))
  ]
 where
  p       = defaultPos
  var     = TypeVar p (mkVar p "a")
  varBind = KindBind p (mkVar p "a") (omission p)

prelude :: VarEnv
prelude = foldl (\acc (x, s) -> Map.insert x s acc) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` map fst typeList)

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))
