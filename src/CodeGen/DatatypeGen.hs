module CodeGen.DatatypeGen (genDataTypes) where

import           Syntax.Programs
import           Syntax.Types
import           Syntax.Position
import           Data.List
import qualified Data.Map.Strict as Map

-- GEN DATATYPES

type DatatypeMap = Map.Map TypeVar [String]

genDataTypes :: ConstructorEnv -> String
genDataTypes cenv =
  Map.foldlWithKey (\acc k v -> acc ++ showDatatype k v ++ "\n") "" (getDataTypeMap cenv)

showDatatype :: TypeVar -> [String] -> String
showDatatype dt xs = "data " ++ dt ++ " = " ++ intercalate " | " xs ++ " deriving Show\n"

getDataTypeMap :: ConstructorEnv -> DatatypeMap
getDataTypeMap = Map.foldlWithKey fun Map.empty
  where
    fun :: DatatypeMap -> Bind -> (Pos, TypeScheme) -> DatatypeMap
    fun dtm (Bind _ cons) (_, t)
      | Map.member (show (last (toList t))) dtm =
           let k = show (last (toList t)) in
           Map.insert k ((dtm Map.! k) ++ [getDataTypes cons t]) dtm
      | otherwise =
           Map.insert (show (last (toList t))) [getDataTypes cons t] dtm


getDataTypes :: TypeVar -> TypeScheme -> String
getDataTypes x t = x ++ foldl (\acc ts -> acc ++ " " ++ show ts) [] (types t)
  where
    types t = init $ toList t
