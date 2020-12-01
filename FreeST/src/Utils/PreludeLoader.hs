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
    (IntType defaultPos)
    (Fun defaultPos Un (IntType defaultPos) (IntType defaultPos))
  )

binBoolOp :: Type
binBoolOp =
  (Fun
    defaultPos
    Un
    (BoolType defaultPos)
    (Fun defaultPos Un (BoolType defaultPos) (BoolType defaultPos))
  )

relationalOp :: Type
relationalOp =
  (Fun
    defaultPos
    Un
    (IntType defaultPos)
    (Fun defaultPos Un (IntType defaultPos) (BoolType defaultPos))
  )

unIntBool :: Type
unIntBool =
  (Fun defaultPos Un (IntType defaultPos) (BoolType defaultPos))

unIntInt :: Type
unIntInt =
  (Fun defaultPos Un (IntType defaultPos) (IntType defaultPos))

unBoolBool :: Type
unBoolBool =
  (Fun defaultPos Un (BoolType defaultPos) (BoolType defaultPos))

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
    , (Fun defaultPos Un (CharType defaultPos) (IntType defaultPos))
    )
  , ( mkVar p "chr"
    , (Fun defaultPos Un (IntType defaultPos) (CharType defaultPos))
    )
  , ( mkVar p "fork"
    , (Fun p Un (UnitType p) (UnitType p))
    )
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", TypeScheme p [KindBind p a (Kind p Functional Lin)] (Fun p Lin (TypeVar p a) (UnitType p))) 
--           , (mkVar p "id", TypeScheme p [TBindK p "a" (Kind p Session Un)] (Fun p Un (TypeVar p "a") (TypeVar p "a")))
  -- Prints
  , (mkVar p "printInt"   , (Fun p Un (IntType p) (UnitType p)))
  , (mkVar p "printIntLn" , (Fun p Un (IntType p) (UnitType p)))
  , (mkVar p "printBool"  , (Fun p Un (BoolType p) (UnitType p)))
  , (mkVar p "printBoolLn", (Fun p Un (BoolType p) (UnitType p)))
  , (mkVar p "printChar"  , (Fun p Un (CharType p) (UnitType p)))
  , (mkVar p "printCharLn", (Fun p Un (CharType p) (UnitType p)))
  , (mkVar p "printUnit"  , (Fun p Un (UnitType p) (UnitType p)))
  , (mkVar p "printUnitLn", (Fun p Un (UnitType p) (UnitType p)))
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
