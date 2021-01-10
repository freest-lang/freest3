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

binIntOp :: TypeScheme
binIntOp = fromType
  (Fun
    defaultPos
    Un
    (Basic defaultPos IntType)
    (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType))
  )

binBoolOp :: TypeScheme
binBoolOp = fromType
  (Fun
    defaultPos
    Un
    (Basic defaultPos BoolType)
    (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))
  )

relationalOp :: TypeScheme
relationalOp = fromType
  (Fun
    defaultPos
    Un
    (Basic defaultPos IntType)
    (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))
  )
  
unIntBool :: TypeScheme
unIntBool = fromType
  (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))

unIntInt :: TypeScheme
unIntInt = fromType
  (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType))

unBoolBool :: TypeScheme
unBoolBool = fromType
  (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))

typeList :: [(ProgVar, TypeScheme)]
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
    , fromType
      (Fun defaultPos Un (Basic defaultPos CharType) (Basic defaultPos IntType))
    )
  , ( mkVar p "chr"
    , fromType
      (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos CharType))
    )
  , ( mkVar p "fork"
    , fromType (Fun p Un (Basic p UnitType) (Basic p UnitType))
    )
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", TypeScheme p [KindBind p a (Kind p Functional Lin)] (Fun p Lin (TypeVar p a) (Basic p UnitType))) 
--           , (mkVar p "id", TypeScheme p [TBindK p "a" (Kind p Session Un)] (Fun p Un (TypeVar p "a") (TypeVar p "a")))
  -- Prints
  , ( mkVar p "printInt"
    , fromType (Fun p Un (Basic p IntType) (Basic p UnitType))
    )
  , ( mkVar p "printIntLn"
    , fromType (Fun p Un (Basic p IntType) (Basic p UnitType))
    )
  , ( mkVar p "printBool"
    , fromType (Fun p Un (Basic p BoolType) (Basic p UnitType))
    )
  , ( mkVar p "printBoolLn"
    , fromType (Fun p Un (Basic p BoolType) (Basic p UnitType))
    )
  , ( mkVar p "printChar"
    , fromType (Fun p Un (Basic p CharType) (Basic p UnitType))
    )
  , ( mkVar p "printCharLn"
    , fromType (Fun p Un (Basic p CharType) (Basic p UnitType))
    )
  , ( mkVar p "printUnit"
    , fromType (Fun p Un (Basic p UnitType) (Basic p UnitType))
    )
  , ( mkVar p "printUnitLn"
    , fromType (Fun p Un (Basic p UnitType) (Basic p UnitType))
    )
  , ( mkVar p "printString"
    , fromType (Fun p Un (Basic p StringType) (Basic p UnitType))
    )
  , ( mkVar p "printStringLn"
    , fromType (Fun p Un (Basic p StringType) (Basic p UnitType))
    )
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
