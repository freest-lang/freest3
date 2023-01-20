module Util.Warning where


import           Syntax.Base
import           Syntax.Program ( TypeOpsEnv )
import qualified Syntax.Type as T
import qualified Syntax.Expression as E
import           Util.Message

import qualified Data.Map                      as Map
import           Data.List                      ( intercalate )
import           System.FilePath


showWarnings :: String -> TypeOpsEnv -> WarningType -> String
showWarnings f tops wrn =
  let base = replaceBaseName f (trimModule f (defModule (getSpan wrn))) in
  title wrn True (getSpan wrn) base ++ "\n  " ++ msg wrn True tops
  where
    trimModule f mod
      | null mod                = takeBaseName f
      | isExtensionOf "fst" mod = takeBaseName mod
      | otherwise               = mod

data WarningType =
    NoPrelude FilePath
  | NonExhaustiveCase Span E.FieldMap T.TypeMap
  deriving Show

instance Located WarningType where
  getSpan (NoPrelude f)             = defaultSpan {defModule = f}
  getSpan (NonExhaustiveCase p _ _) = p

instance Message WarningType where
  title _  sty = msgHeader (yellow sty "warning:") sty
  
  msg NoPrelude{} _ _ = "Could not load the Prelude; proceeding without it"
  msg (NonExhaustiveCase _ fm tm) sty _ =
    "Pattern match(es) are non-exhaustive\n\t In a case alternative: Patterns not matched: " ++
    yellow sty (intercalate ", " $ map show $ Map.keys $ Map.difference tm fm)
