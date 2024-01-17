module Util.Warning where

import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.Program ( TypeOpsEnv )
import qualified Syntax.Type as T
import           Util.Message

import           Data.List ( intercalate )
import qualified Data.Map as Map


showWarnings :: Stylable -> String -> TypeOpsEnv -> WarningType -> String
showWarnings sty f tops wrn =
  title wrn sty (getSpan wrn) f ++ "\n  " ++ msg wrn sty tops
  
data WarningType =
    NoPrelude FilePath
  | NonExhaustiveCase Span E.FieldMap T.TypeMap
  deriving Show

instance Located WarningType where
  getSpan (NoPrelude f)             = defaultSpan {moduleName = f}
  getSpan (NonExhaustiveCase p _ _) = p

instance Message WarningType where
  title _  sty = msgHeader (yellow sty "warning:") sty
  
  msg NoPrelude{} _ _ = "Could not load the Prelude; proceeding without it"
  msg (NonExhaustiveCase _ fm tm) sty _ =
    "Pattern match(es) are non-exhaustive\n\t In a case alternative: Patterns not matched: " ++
    yellow sty (intercalate ", " $ map show $ Map.keys $ Map.difference tm fm)
