module Util.Warning where


import           Syntax.Base
import           Syntax.Program ( TypeOpsEnv )
import qualified Syntax.Type as T
import qualified Syntax.Expression as E
-- import           Util.PrettyWarning
-- import           Util.WarningMessage
import           Util.Message

import qualified Data.Map                      as Map
import           Data.List                      ( intercalate )
import           Prelude hiding (span)
import           System.FilePath

-- TODO: Stylable to runOpts

showWarnings :: String -> TypeOpsEnv -> WarningType -> String
showWarnings f tops wrn = let base = replaceBaseName f (defModule (span wrn)) in
  title wrn True (span wrn) base ++ "\n  " ++ msg wrn True tops


data WarningType =
    NoPrelude 
  | NonExhaustiveCase Span E.FieldMap T.TypeMap
  deriving Show

instance Spannable WarningType where
  span NoPrelude                 = defaultSpan
  span (NonExhaustiveCase p _ _) = p


-- warningMsg :: WarningType -> [WarningMessage]
-- warningMsg (NonExhaustiveCase _ fm tm) =
--   [ Warning "Pattern match(es) are non-exhaustive\n\t"
--   , Warning "In a case alternative: Patterns not matched:"
--   , Warning $ formatColor (Just Pink) $ foldr (\k acc -> show k ++ acc) ""
--             $ Map.keys $ Map.difference tm fm]


instance Message WarningType where
  title _  sty = msgHeader (yellow sty "warning:") sty
  
  msg NoPrelude _ _ = "Couldn't find prelude; proceeding without it"
  msg (NonExhaustiveCase _ fm tm) sty _ =
    "Pattern match(es) are non-exhaustive\n\t In a case alternative: Patterns not matched:" ++
    yellow sty (intercalate ", " $ map show $ Map.keys $ Map.difference tm fm)
