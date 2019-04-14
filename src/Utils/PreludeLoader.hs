module Utils.PreludeLoader
( prelude
, isBuiltin
) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map

binIntOp = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType)))
binBoolOp = toTypeScheme (Fun defaultPos Un (Basic defaultPos BoolType) (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType)))
relationalOp = toTypeScheme(Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType)))
unIntBool = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))
unIntInt = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType)  (Basic defaultPos IntType))
unBoolBool = toTypeScheme (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))

typeList :: [(ProgVar, TypeScheme)]
typeList = [ (mkProgVar defaultPos "(+)", binIntOp)
           , (mkProgVar defaultPos "(-)", binIntOp)
           , (mkProgVar defaultPos "(/)", binIntOp)
           , (mkProgVar defaultPos "(*)", binIntOp)
           , (mkProgVar defaultPos "mod", binIntOp)
           , (mkProgVar defaultPos "rem", binIntOp)
           , (mkProgVar defaultPos "div", binIntOp)
           , (mkProgVar defaultPos "negate", unIntInt)
           , (mkProgVar defaultPos "not", unBoolBool)
           , (mkProgVar defaultPos "(&&)", binBoolOp)
           , (mkProgVar defaultPos "(||)", binBoolOp)
           , (mkProgVar defaultPos "(==)", relationalOp)
           , (mkProgVar defaultPos "(<)", relationalOp)
           , (mkProgVar defaultPos "(>)", relationalOp)
           , (mkProgVar defaultPos "(<=)", relationalOp)
           , (mkProgVar defaultPos "(>=)", relationalOp)
--           , (mkProgVar defaultPos "id", TypeScheme defaultPos [TBindK defaultPos "a" (Kind defaultPos Session Un)] (Fun defaultPos Un (TypeVar defaultPos "a") (TypeVar defaultPos "a")))
           ]

prelude :: VarEnv
prelude = foldl (\acc (x, s) -> Map.insert x s acc) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` (map fst typeList))

-- preludeLoad :: VarEnv -> VarEnv
-- preludeLoad venv = foldl (\acc (x, s) -> Map.insert x s acc) venv typeList


{-
preludeLoad :: VarEnv -> VarEnv
preludeLoad map =
  foldl (\acc (tv, t) ->
     Map.insert (PBind defaultPos tv)
                (read t :: TypeScheme) acc) map typeList

schemeList :: [(String, String)]
schemeList = [
--  ("id", "forall a : SU => a -> a")
  -- ("fst", "forall a, b => (a, b) -> a") -- fst/snd applies only to un-pairs but our pairs are lin
  ]
schemeLoad :: VarEnv -> VarEnv
schemeLoad map =
  foldl (\acc (tv, t) -> Map.insert (PBind defaultPos tv) (read t :: TypeScheme) acc) map schemeList
-}     



-- isBinOpApp :: Expression -> Bool
-- isBinOpApp (App _ e1 e2) = isBinOpApp e1 || isBinOpApp e2
-- isBinOpApp (ProgVar p x) = isBinOp (PBind p x)
-- isBinOpApp _             = False

-- isBinOp :: PBind -> Bool
-- isBinOp (PBind _ x) =
--   case x `lookup` typeList of
--     Just t -> 3 == (length (toListT  t))
--     Nothing -> False


-- toListT :: Type -> [Type]
-- toListT (Fun _ _ t1 t2) = t1 : toListT t2
-- toListT t = [t]


