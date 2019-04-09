module Utils.PreludeLoader
( prelude
, isBuiltin
, isBinOpApp
) where

import           Parse.Lexer (defaultPos)
import           Syntax.Programs (VarEnv)
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Bind
import           Syntax.Kinds (Multiplicity(..))
import           Syntax.Expression
import           Parse.Parser
import qualified Data.Map.Strict as Map


binInt = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos IntType)))
binBool = toTypeScheme (Fun defaultPos Un (Basic defaultPos BoolType) (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType)))
relationalOps = toTypeScheme(Fun defaultPos Un (Basic defaultPos IntType) (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType)))
unIntBool = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType) (Basic defaultPos BoolType))
unIntInt = toTypeScheme (Fun defaultPos Un (Basic defaultPos IntType)  (Basic defaultPos IntType))
unBoolBool = toTypeScheme (Fun defaultPos Un (Basic defaultPos BoolType) (Basic defaultPos BoolType))


typeList :: [(String, TypeScheme)]
typeList = [ ("(+)",  binInt)
           , ("(-)", binInt)
           , ("(/)", binInt)
           , ("(*)", binInt)
           , ("mod", binInt)
           , ("rem", binInt)
           , ("div", binInt)
           , ("negate", unIntInt)
           , ("not", unBoolBool)
           , ("(&&)", binBool)
           , ("(||)", binBool)
           , ("(==)", relationalOps)
           , ("(<)", relationalOps)
           , ("(>)", relationalOps)
           , ("(<=)", relationalOps)
           , ("(>=)", relationalOps)
           , ("id", TypeScheme [TBindK defaultPos "a" (Kind defaultPos Session Un)] (Fun defaultPos Un (Var defautPos "a") (Var defautPos "a")))
           ]

prelude :: VarEnv
prelude = preludeLoad Map.empty
  -- schemeLoad (preludeLoad Map.empty)

preludeLoad :: VarEnv -> VarEnv
preludeLoad venv = 
  foldl (\acc (tv, t) -> Map.insert (PBind defaultPos tv) (TypeScheme defaultPos [] t) acc) venv typeList

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

isBuiltin :: PBind -> Bool
isBuiltin (PBind _ x) = x `elem` (map fst typeList)


isBinOpApp :: Expression -> Bool
isBinOpApp (App _ e1 e2) = isBinOpApp e1 || isBinOpApp e2
isBinOpApp (ProgVar p x) = isBinOp (PBind p x)
isBinOpApp _             = False

isBinOp :: PBind -> Bool
isBinOp (PBind _ x) =
  case x `lookup` typeList of
    Just t -> 3 == (length (toListT  t))
    Nothing -> False


toListT :: Type -> [Type]
toListT (Fun _ _ t1 t2) = t1 : toListT t2
toListT t = [t]


