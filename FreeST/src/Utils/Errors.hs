{-|
Module      :  Utils.Errors
Description :  This module provides tools to prettify & format errors
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

This module provides tools to prettify & format errors with ANSI colors for terminals
-}

module Utils.Errors
  ( formatErrorMessages
  , internalError
  )
where

import           Syntax.Base                    ( Pos, Position, position )
import           Syntax.Types                   ( TypeOpsEnv )
import           Parse.Unparser
import           Utils.ErrorMessage

-- | Format errors
formatErrorMessages :: TypeOpsEnv -> Pos -> String -> [ErrorMessage] -> String
formatErrorMessages _ _ _ [] = ""
formatErrorMessages tops p fname es =
  let header = styleHeader fname p
      body   = foldl (\acc e -> acc ++ " " ++ colorMsg tops e) "" es
  in  header ++ body

colorMsg :: TypeOpsEnv -> ErrorMessage -> String
colorMsg tops (Error e) = styleColor (color e) (boldMsg tops e)

boldMsg :: ErrorMsg a => TypeOpsEnv -> a -> String
boldMsg tops m = styleBold (msg tops m)

-- Style the error header

styleHeader :: String -> Pos -> String
styleHeader f p =
  styleBold $ "\n" ++ f ++ ":" ++ show p ++ ": " ++ styleRed "error:\n\t"

-- Style colors, this is built in from now on
-- instead of importing System.Console.Pretty

styleRed :: String -> String
styleRed str = "\ESC[91m" ++ str ++ "\ESC[0m"

styleBold :: String -> String
styleBold str = "\ESC[1m" ++ str ++ "\ESC[0m"

styleColor :: Maybe Color -> String -> String
styleColor (Just Red) str = styleRed str
styleColor _          str = str

-- | Internal errors

internalError :: (Show a, Position a) => String -> a -> b
internalError fun syntax = do
  error $ show (position syntax) ++ ": Internal error at " ++ fun ++ ": " ++ show syntax

