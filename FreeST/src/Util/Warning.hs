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
import           Data.Maybe
import           Parse.Unparser                 ( showFieldMap ) -- temporary


formatWarning:: Maybe String -> TypeOpsEnv -> WarningType -> String
formatWarning mFile tops wrn = format (pos wrn) (warningMsg wrn)
  where
   f = fromMaybe "FreeST" mFile
   format p e = formatBody tops e


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
  [ Warning "Wrong number of constructors\n\tThe expression has", Warning $ Map.size fm
  , Warning "constructor(s)\n\tbut the type has", Warning $ Map.size tm
  , Warning "constructor(s)\n\tin case "
  , Warning $ "\ESC[36m{" ++ showFieldMap fm ++ "}\ESC[0m"]
