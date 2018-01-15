module SpecHelper
    ( module Test.Hspec
    , module Types.Types
    , module Types.Parser
    , readFromFile
    , convert
    , module Data.Char
    ) where

import Test.Hspec
import Types.Types
import Data.Char
import Types.Parser

readFromFile filename = do
  str <- readFile filename
  -- print $ filter (not . null) $ lines str
  return $ filter (not . null) $ map (dropWhile isSpace) $ lines str
  -- return $ map (dropWhile isSpace) $ lines str

convert :: [String] -> [(String, String)]
convert [] = []
convert (k:v:t) = (k, v) : convert t
-- convert t = error $ "ERRO: " ++ show t
