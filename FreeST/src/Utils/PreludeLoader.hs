{- |
Module      :  Syntax.Types
Description :  Signatures for the functions in the prelude
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

This module introduces the signatures for the functions in the prelude
-}

module Utils.PreludeLoader
  ( prelude
  , isBuiltin
  , userDefined
  )
where

import           Syntax.ProgramVariable
import           Syntax.Base
import qualified Syntax.Type                   as T
import           Syntax.Program
import qualified Data.Map.Strict               as Map
import           Parse.Read                     ()

typeList :: [(ProgVar, T.Type)]
typeList =
  [ (mkVar p "(+)"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "(-)"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "(/)"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "(*)"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "mod"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "rem"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "div"   , read "Int -> Int -> Int"    :: T.Type)
  , (mkVar p "negate", read "Int -> Int"           :: T.Type)
  , (mkVar p "not"   , read "Bool -> Bool"         :: T.Type)
  , (mkVar p "(&&)"  , read "Bool -> Bool -> Bool" :: T.Type)
  , (mkVar p "(||)"  , read "Bool -> Bool -> Bool" :: T.Type)
  , (mkVar p "(==)"  , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "(/=)"  , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "(<)"   , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "(>)"   , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "(<=)"  , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "(>=)"  , read "Int -> Int -> Bool"   :: T.Type)
  , (mkVar p "ord"   , read "Char -> Int"          :: T.Type)
  , (mkVar p "chr"   , read "Int -> Char"          :: T.Type)
  , (mkVar p "fork"  , read "() -> ()"             :: T.Type)
  -- , (mkVar p "fst"   , read "∀ a:TU => ∀ b:TU => (a, b) -> a" :: T.Type)
  -- , (mkVar p "snd"   , read "∀ a:TU => ∀ b:TU => (a, b) -> b" :: T.Type)
  , (mkVar p "printInt"   , read "Int -> ()"       :: T.Type)
  , (mkVar p "printIntLn" , read "Int -> ()"       :: T.Type)
  , (mkVar p "printBool"  , read "Bool -> ()"      :: T.Type)
  , (mkVar p "printBoolLn", read "Bool -> ()"      :: T.Type)
  , (mkVar p "printChar"  , read "Char -> ()"      :: T.Type)
  , (mkVar p "printCharLn", read "Char -> ()"      :: T.Type)
  , (mkVar p "printUnit"  , read "() -> ()"        :: T.Type)
  , (mkVar p "printUnitLn", read "() -> ()"        :: T.Type)
  ]
  where p = defaultPos
  
prelude :: VarEnv
prelude = foldr (uncurry Map.insert) Map.empty typeList

isBuiltin :: ProgVar -> Bool
isBuiltin = (`elem` map fst typeList)

userDefined :: VarEnv -> VarEnv
userDefined = Map.filterWithKey (\x _ -> not (isBuiltin x))
