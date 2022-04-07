{-# LANGUAGE FlexibleInstances #-}
module SpecUtils
  ( module Test.Hspec
  , module Syntax.Type
  , module Parse.Parser
  , module Parse.Read
  , module Data.List.Split
  , module Data.Char
  , readFromFile
  , readKenv
  , TestExpectation
  , testValidExpectation
  )
where

import           Test.Hspec
import           Syntax.Type
import           Data.Char
import           Parse.Parser
import Parse.Read
import           Data.List                      ( intercalate )
import           Data.List.Split                ( chunksOf )
import qualified Data.Map.Strict               as Map
import           Syntax.Kind                   ( KindEnv )
import           Syntax.Base                   ( defaultPos
                                               , mkVar
                                               , defaultSpan
                                               )
import           Util.FreestState              ( Errors )
import           Util.Error
-- import Debug.Trace

readFromFile :: FilePath -> IO [String]
readFromFile filename = do
  str <- readFile filename
  return
    $ filter (not . isComment)
    $ filter (not . null)
    $ map (dropWhile isSpace)
    $ lines str
  where
    isComment ('-' : '-' : _) = True
    isComment _               = False

readKenv :: String -> KindEnv
readKenv s =
  Map.fromList $ map (\(x, k) -> {-trace (x ++ "\t" ++ k) $-} (mkVar defaultSpan x, read k)) (read s)



-- Test expectations

-- WAS: type Expect = Either Bool String
-- type Expect = Maybe Errors

-- instance {-# OVERLAPPING #-} Show Expect where
--   show Nothing = "True" -- Expect valid
--   show (Just err) = showErrors err

-- showErrors :: [String] -> String
-- showErrors = intercalate "\n" . take 2

-- testValidExpectation :: Errors -> Expect
-- testValidExpectation errs
--   | not $ null errs = Just errs
--   | otherwise       = Nothing

-- expect :: Expect
-- expect = Nothing


type TestExpectation = Either Bool String

instance {-# OVERLAPPING #-} Show TestExpectation where
  show (Left b)    = show b
  show (Right err) = err

showTestErrors :: Errors -> String
showTestErrors = intercalate "\n" . map f . take 2 . reverse
  where f = showErrors "" Map.empty

testValidExpectation :: Bool -> Errors -> TestExpectation
testValidExpectation b errs
  | not $ null errs = Right $ showTestErrors errs
  | otherwise       = Left b
