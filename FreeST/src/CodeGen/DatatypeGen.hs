{-# LANGUAGE OverloadedStrings #-}
module CodeGen.DatatypeGen
( genDataTypes
) where

import           Syntax.Types
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Show
import           Data.List
import qualified Data.Map.Strict as Map

-- genDataTypes :: TypeEnv -> String
-- genDataTypes tenv = "" --Map.foldlWithKey (\acc k v -> acc ++ showElem k v ++ "\n") ""


-- GEN DATATYPES

genDataTypes :: TypeEnv -> String
genDataTypes = Map.foldlWithKey (\acc k v -> acc ++ showElem k (snd v) ++ "\n") ""

showElem :: TypeVar -> TypeScheme -> String
showElem x (TypeScheme _ _ (Datatype _ m)) = showDatatype x m
showElem x (TypeScheme _ _ _)              = ""
-- TODO: removed the generation of types... add later
-- showElem x (TypeScheme _ _ t)              = showTypeAbbr x t

showDatatype :: TypeVar -> TypeMap -> String
showDatatype x m = "data " ++ show x ++ " = " ++ showDatatypeMap m ++ " deriving Show"

showDatatypeMap :: TypeMap -> String
showDatatypeMap m =
  intercalate " | " $ Map.foldrWithKey (\b t hc -> (show b ++ " " ++ showTypes t) : hc) [] m

showTypes :: Type -> String
showTypes = intercalate " " . map show . init . toListT

showTypeAbbr :: TypeVar -> Type -> String
showTypeAbbr x t = "type " ++ show x ++ " = " ++ show t

-- TODO: Remove, do I need toList over schemes or only types
toListT :: Type -> [Type]
toListT (Fun _ _ t1 t2) = t1 : toListT t2
toListT t = [t]

