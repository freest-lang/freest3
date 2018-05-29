module SpecHelper
    ( module Test.Hspec
    , module Types.Types
    , module Types.TypeParser
    , module Data.List.Split
    , module Data.Char
    , readFromFile
    ) where

import Test.Hspec
import Types.Types
import Data.Char
import Types.TypeParser
import Data.List.Split(chunksOf)

readFromFile filename = do
  str <- readFile filename
  -- print $ filter (not . null) $ lines str
  return $ filter (not . isComment) $ filter (not . null) $ map (dropWhile isSpace) $ lines str
  -- return $ map (dropWhile isSpace) $ lines str

isComment (x:y:ys) = x == '-' && y == '-'
isComment _ = False
