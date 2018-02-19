module SpecHelper
    ( module Test.Hspec
    , module Types.Types
    , module Types.TypeParser
    , readFromFile
    , convert
    , module Data.Char
    ) where

import Test.Hspec
import Types.Types
import Data.Char
import Types.TypeParser

readFromFile filename = do
  str <- readFile filename
  -- print $ filter (not . null) $ lines str
  return $ filter (not . isComment) $ filter (not . null) $ map (dropWhile isSpace) $ lines str
  -- return $ map (dropWhile isSpace) $ lines str

isComment (x:y:ys) = x == '-' && y == '-'
isComment _ = False

convert :: [String] -> [(String, String)]
convert [] = []
convert (k:v:t) = (k, v) : convert t
-- convert t = error $ "ERRO: " ++ show t
