module Util.Warning
    ( WarningType(..)
    , formatWarning
    ) where


import           Syntax.Base
import           Syntax.Program                 ( TypeOpsEnv )
import qualified Syntax.Type                   as T
import qualified Syntax.Expression             as E
import           Util.PrettyWarning
import           Util.WarningMessage
import qualified Data.Map                      as Map
-- import           Data.Maybe

formatWarning :: String -> TypeOpsEnv -> WarningType -> String
formatWarning f tops wrn = format (pos wrn) (warningMsg wrn)
  where
   format p e = formatHeader f p ++ formatBody tops e


-- Warnings
-- FreeST.hs:82:
   -- styleCyan "warning: " ++ "Couldn't find prelude; proceeding without it"
-- CmdLine:72:
   -- multiple files provided

data WarningType =
  NonExhaustiveCase Pos E.FieldMap T.TypeMap
  deriving Show

instance Position WarningType where
  pos (NonExhaustiveCase p _ _) = p

warningMsg :: WarningType -> [WarningMessage]
warningMsg (NonExhaustiveCase _ fm tm) =
  [ Warning "Pattern match(es) are non-exhaustive\n\t"
  , Warning "In a case alternative: Patterns not matched:"
  , Warning $ formatColor (Just Pink) $ foldr (\k acc -> show k ++ acc) ""
            $ Map.keys $ Map.difference tm fm]
