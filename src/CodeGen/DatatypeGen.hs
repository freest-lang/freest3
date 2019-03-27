module CodeGen.DatatypeGen (genDataTypes) where

import           Syntax.Programs
import           Syntax.Types
import           Syntax.Bind
import           Data.List
import qualified Data.Map.Strict as Map

-- GEN DATATYPES

type DatatypeMap = Map.Map TypeVar [String]

genDataTypes :: TypeEnv -> String
genDataTypes cenv =
  Map.foldlWithKey (\acc k v -> acc ++ showDatatype k v ++ "\n") "" (getDataTypeMap cenv)

showDatatype :: TypeVar -> [String] -> String
showDatatype dt xs = "data " ++ dt ++ " = " ++ intercalate " | " xs ++ " deriving Show\n"

getDataTypeMap :: TypeEnv -> DatatypeMap
getDataTypeMap = Map.foldlWithKey fun Map.empty
  where
    fun :: DatatypeMap -> KBind -> TypeScheme -> DatatypeMap
    fun dtm (KBind _ cons top) t
      | Map.member (show (last (toList t))) dtm =
           let k = show (last (toList t)) in
           Map.insert k ((dtm Map.! k) ++ [getDataTypes cons t]) dtm
      | otherwise =
           Map.insert (show (last (toList t))) [getDataTypes cons t] dtm


getDataTypes :: TypeVar -> TypeScheme -> String
getDataTypes x t = x ++ foldl (\acc ts -> acc ++ " " ++ show ts) [] (types t)
  where
    types t = init $ toList t
