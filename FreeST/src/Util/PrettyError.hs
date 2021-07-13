{-|
Module      :  Util.Error
Description :  This module provides tools to prettify & format errors
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

This module provides tools to prettify & format errors with ANSI colors for terminals
-}

module Util.PrettyError
  ( -- formatErrorMessage,
    formatColor
  , formatBold
  , formatHeader
  , formatBody
  )
where

import           Syntax.Base                    ( Pos
                                                , pos
                                                , defaultPos
                                                )
import           Syntax.Program                 ( TypeOpsEnv )
import           Util.ErrorMessage


-- | Style of the error header

formatHeader :: String -> Pos -> String
formatHeader f p
  | p == defaultPos = formatBold $ start ++ end
  | otherwise       = formatBold $ start ++ ":" ++ show p ++ end
 where
  start = "\n" ++ f
  end   = ": " ++ formatColor (Just Red) "error:\n\t"

-- | Style of the error body
formatBody :: TypeOpsEnv -> [ErrorMessage] -> String
formatBody tops = foldl (\acc e -> acc ++ " " ++ colorMsg e) ""
  where
    colorMsg :: ErrorMessage -> String
    colorMsg (Error e) = formatColor (color e) (formatBold $ msg tops e)

-- | Style colors, this is built in from now on
-- instead of importing System.Console.Pretty

formatBold :: String -> String
formatBold str = "\ESC[1m" ++ str ++ "\ESC[0m"

formatColor :: Maybe Color -> String -> String
formatColor (Just Red) str = "\ESC[91m" ++ str ++ "\ESC[0m"
formatColor (Just Cyan) str = "\ESC[36m" ++ str ++ "\ESC[0m"
formatColor _          str = str
