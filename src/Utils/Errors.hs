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

module Utils.Errors (styleError, styleRed, prettyPos) where 

import System.Console.Pretty (Color (..), Style (..), color, style)
import Syntax.Position (Pos, AlexPosn(..))

styleError :: String -> Pos -> [String] -> String
styleError f p body = styleHeader f p ++ styleBody body

styleBody :: [String] -> String
styleBody = foldl (\acc x -> acc ++ " " ++ styleBold x) "" 

styleHeader :: String -> Pos -> String
styleHeader f p =
  styleBold $ "\n" ++ f ++ ":" ++ prettyPos p ++ ": " ++ styleRed "error:\n\t"

styleRed :: String -> String
styleRed = color Red

styleBold :: String -> String  
styleBold = style Bold

prettyPos :: Pos -> String
prettyPos (AlexPn _ px py) = show px ++ ":" ++ show py

