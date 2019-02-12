module SpecHelper
    ( module Test.Hspec
    , module Syntax.Types
    , module Parse.Parser
    , module Data.List.Split
    , module Data.Char
    , readFromFile
    ) where

import Test.Hspec
import Syntax.Types
import Data.Char
import Parse.Parser
import Data.List.Split(chunksOf)

readFromFile filename = do
  str <- readFile filename
  -- print $ filter (not . null) $ lines str
  return $ filter (not . isComment) $ filter (not . null) $ map (dropWhile isSpace) $ lines str
  -- return $ map (dropWhile isSpace) $ lines str

isComment ('-':'-':_) = True
isComment _           = False
