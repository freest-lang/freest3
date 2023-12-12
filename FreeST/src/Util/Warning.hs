module Util.Warning where

import           Parse.Unparser (showModuleName)
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.Program ( TypeOpsEnv )
import qualified Syntax.Type as T
import           Util.Message

import           Data.Either.Extra (fromEither , isLeft)
import           Data.List ( intercalate )
import qualified Data.Map as Map
import           System.FilePath


showWarnings :: Stylable -> String -> TypeOpsEnv -> WarningType -> String
showWarnings sty f tops wrn =
  let mod = trimModule f (moduleName $ getSpan wrn) in
  let base = replaceBaseName f (fromEither mod) in
  let modEither = if isLeft mod then Left base else Right $ showModuleName (getSpan wrn) in    
    title wrn sty (getSpan wrn) base ++ "\n  " ++ msg wrn sty tops modEither
  where
    trimModule f mod
      | null mod                = Left $ takeBaseName f
      | isExtensionOf "fst" mod = Left $ takeBaseName mod
      | otherwise               = Right mod

data WarningType =
    NoPrelude FilePath
  | NonExhaustiveCase Span E.FieldMap T.TypeMap
  deriving Show

instance Located WarningType where
  getSpan (NoPrelude f)             = defaultSpan {moduleName = f}
  getSpan (NonExhaustiveCase p _ _) = p

instance Message WarningType where
  title _  sty = msgHeader (yellow sty "warning:") sty
  
  msg NoPrelude{} _ _ _ = "Could not load the Prelude; proceeding without it"
  msg (NonExhaustiveCase _ fm tm) sty _ _ =
    "Pattern match(es) are non-exhaustive\n\t In a case alternative: Patterns not matched: " ++
    yellow sty (intercalate ", " $ map show $ Map.keys $ Map.difference tm fm)
