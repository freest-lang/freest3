module SpecHelper
  ( module Test.Hspec
  , module Syntax.Types
  , module Parse.Parser
  , module Parse.Read
  , module Data.List.Split
  , module Data.Char
  , readFromFile
  , readKenv
  )
where

import           Test.Hspec
import           Syntax.Types
import           Data.Char
import           Parse.Parser
import Parse.Read
import           Data.List.Split                ( chunksOf )
import qualified Data.Map.Strict               as Map
import           Syntax.Kind                   ( KindEnv )
import           Syntax.Base                    ( defaultPos
                                                , mkVar
                                                )


readFromFile filename = do
  str <- readFile filename
  return
    $ filter (not . isComment)
    $ filter (not . null)
    $ map (dropWhile isSpace)
    $ lines str

isComment ('-' : '-' : _) = True
isComment _               = False

readKenv :: String -> KindEnv
readKenv s =
  Map.fromList $ map (\(x, k) -> (mkVar defaultPos x, read k)) (read s)
