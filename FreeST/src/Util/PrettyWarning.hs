module Util.PrettyWarning
    ( formatHeader
    , formatBody
    , formatBold
    , formatColor
    ) where

import           Syntax.Base                    ( Pos
                                                , pos
                                                , defaultPos
                                                )
import           Syntax.Program                 ( TypeOpsEnv )
import           Util.WarningMessage
import           Parse.Unparser                 -- show Pos

-- | Style of the warning header

formatHeader :: String -> Pos -> String
formatHeader f p
  | p == defaultPos = formatBold $ start ++ end
  | otherwise       = formatBold $ start ++ ":" ++ show p ++ end
 where
  start = "\n" ++ f
  end   = ": " ++ formatColor (Just Pink) "warning:\n\t"

-- | Style of the warning body
formatBody :: TypeOpsEnv -> [WarningMessage] -> String
formatBody tops = foldl (\acc e -> acc ++ " " ++ colorMsg e) ""
  where
    colorMsg :: WarningMessage -> String
    colorMsg (Warning e) = formatColor (color e) (formatBold $ msg tops e)

-- | Style colors, this is built in from now on
-- instead of importing System.Console.Pretty

formatBold :: String -> String
formatBold str = "\ESC[1m" ++ str ++ "\ESC[0m"

formatColor :: Maybe Color -> String -> String
formatColor (Just Red)  str = "\ESC[91m" ++ str ++ "\ESC[0m"
formatColor (Just Pink) str = "\ESC[95m" ++ str ++ "\ESC[0m"
formatColor _           str = str
