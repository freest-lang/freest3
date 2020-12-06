module Utils.PreludeLoader
  ( prelude
  , isBuiltin
  , userDefined
  )
where

-- import           Syntax.Schemes
import qualified Syntax.Type                   as T
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict               as Map
-- import           Syntax.Kind

binIntOp :: T.Type
binIntOp =
  T.Fun defaultPos
         Un
         (T.IntType defaultPos)
         (T.Fun defaultPos Un (T.IntType defaultPos) (T.IntType defaultPos))
  

binBoolOp :: T.Type
binBoolOp =
  T.Fun defaultPos
         Un
         (T.BoolType defaultPos)
         (T.Fun defaultPos Un (T.BoolType defaultPos) (T.BoolType defaultPos))
  

relationalOp :: T.Type
relationalOp =
  T.Fun defaultPos
         Un
         (T.IntType defaultPos)
         (T.Fun defaultPos Un (T.IntType defaultPos) (T.BoolType defaultPos))
  

-- unIntBool :: T.Type
-- unIntBool =
--   T.Fun defaultPos Un (T.IntType defaultPos) (T.BoolType defaultPos)

unIntInt :: T.Type
unIntInt = T.Fun defaultPos Un (T.IntType defaultPos) (T.IntType defaultPos)

unBoolBool :: T.Type
unBoolBool =
  T.Fun defaultPos Un (T.BoolType defaultPos) (T.BoolType defaultPos)

typeList :: [(ProgVar, T.Type)]
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
    , T.Fun defaultPos Un (T.CharType defaultPos) (T.IntType defaultPos)
    )
  , ( mkVar p "chr"
    , T.Fun defaultPos Un (T.IntType defaultPos) (T.CharType defaultPos)
    )
  , ( mkVar p "fork"
    , T.Fun p Un (T.UnitType p) (T.UnitType p)
    )
-- If introduce fork here, programs must instantiate ths poly var. E.g., 'fork [()] (boolServer r)'
--  , (mkVar p "fork", T.TypeScheme p [KindBind p a (Kind p T.Functional Lin)] (T.Fun p Lin (TypeVar p a) (T.UnitType p))) 
--           , (mkVar p "id", T.TypeScheme p [TBindK p "a" (Kind p Session Un)] (T.Fun p Un (TypeVar p "a") (TypeVar p "a")))
  -- Prints
  , (mkVar p "printInt"   , T.Fun p Un (T.IntType p) (T.UnitType p))
  , (mkVar p "printIntLn" , T.Fun p Un (T.IntType p) (T.UnitType p))
  , (mkVar p "printBool"  , T.Fun p Un (T.BoolType p) (T.UnitType p))
  , (mkVar p "printBoolLn", T.Fun p Un (T.BoolType p) (T.UnitType p))
  , (mkVar p "printChar"  , T.Fun p Un (T.CharType p) (T.UnitType p))
  , (mkVar p "printCharLn", T.Fun p Un (T.CharType p) (T.UnitType p))
  , (mkVar p "printUnit"  , T.Fun p Un (T.UnitType p) (T.UnitType p))
  , (mkVar p "printUnitLn", T.Fun p Un (T.UnitType p) (T.UnitType p))
  ]
  where p = defaultPos
  -- var     = T.TypeVar p (mkVar p "a")
  -- varBind = KindBind p (mkVar p "a") (omission p)

prelude :: T.VarEnv
prelude = foldl (\acc (x, s) -> Map.insert x s acc) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` map fst typeList)

userDefined :: T.VarEnv -> T.VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))
