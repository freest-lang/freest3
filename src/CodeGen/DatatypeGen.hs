{-# LANGUAGE OverloadedStrings #-}
module CodeGen.DatatypeGen
( genDataTypes
) where

import           Syntax.Programs
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Data.List
import qualified Data.Map.Strict as Map

-- GEN DATATYPES

genDataTypes :: TypeEnv -> String
genDataTypes = Map.foldlWithKey (\acc k v -> acc ++ showElem k v ++ "\n") ""

showElem :: TBind -> (Kind, TypeScheme) -> String
showElem (TBind _ x) (_, (TypeScheme _ _ (Datatype _ m))) = showDatatype x m
showElem (TBind _ x) (_, (TypeScheme _ _ t))              = showTypeAbbr x t


showDatatype :: TVar -> TypeMap -> String
showDatatype x m = "data " ++ x ++ " = " ++ showDatatypeMap m ++ " deriving Show"

showDatatypeMap :: TypeMap -> String
showDatatypeMap m =
  intercalate " | " $ Map.foldrWithKey (\b t hc -> (show b ++ " " ++ showTypes t) : hc) [] m

showTypes :: Type -> String
showTypes = intercalate " " . map show . init . toListT

showTypeAbbr :: TVar -> Type -> String
showTypeAbbr x t = "type " ++ x ++ " = " ++ show t


-- TODO: Remove, do I need toList over schemes or only types
toListT :: Type -> [Type]
toListT (Fun _ _ t1 t2) = t1 : toListT t2
toListT t = [t]
