{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax.AST where

import           Syntax.Base
import           Syntax.MkName
import qualified Syntax.Kind as K
import qualified Syntax.Type as T

import qualified Data.Map as Map
import Data.Bifunctor

type family XDef a -- = [([E.Pattern], E.Exp)]

-- type TypeEnv = Map.Map Variable (K.Kind, T.Type)
type Types = Map.Map Variable (K.Kind, T.Type)

-- type VarEnv = Map.Map Variable T.Type
type Signatures = Map.Map Variable T.Type

-- type ParseEnvPat = Map.Map Variable [([Pattern], Exp)]
-- type ParseEnv    = Map.Map Variable ([Variable], Exp)
-- type Prog = Map.Map Variable E.Exp
type Definitions a = Map.Map Variable (XDef a)
  
--  moduleName :: Maybe FilePath

data AST a = AST
  { types       :: Types
  , signatures  :: Signatures
  , definitions :: Definitions a
  } 

initialAST :: AST a
initialAST = AST
  { types       = initialTypes
  , signatures  = initialSigs
  , definitions = Map.empty
  }

addSignature :: Variable -> T.Type -> AST a -> AST a
addSignature f t ast = ast{signatures = Map.insert f t (signatures ast)} 

addType :: Variable -> K.Kind -> T.Type -> AST a -> AST a
addType x k t ast = ast{types = Map.insert x (k,t) (types ast)}

setASTTypes :: Types -> AST a -> AST a
setASTTypes types ast = ast{types}

-- Generic add definition, depends on the instantiation of the type family XDef.
-- In some phases we may need a more specific implementation
addDefinition :: Variable -> XDef a -> AST a -> AST a
addDefinition x d ast = ast{definitions = Map.insert x d (definitions ast)}

setDefinitions :: Definitions a -> AST a -> AST a
setDefinitions definitions ast = ast{definitions}

setSigs :: Signatures -> AST a -> AST a
setSigs signatures ast = ast{signatures}

removeSig :: Variable -> AST a -> AST a
removeSig x ast = ast{signatures = Map.delete x (signatures ast)}


-- | Lists

ds :: Span
ds = defaultSpan

initialTypes :: Types
initialTypes = Map.singleton (mkList ds) (K.ut ds, listType)

initialSigs :: Signatures
initialSigs = Map.fromList listTypes

listTypes :: [(Variable, T.Type)]
listTypes = typeListToType (mkList ds)
              [(mkCons ds,[T.Int ds, T.Var ds (mkList ds)]), (mkNil ds, [])]

listType :: T.Type
listType = T.Labelled ds T.Variant (typeListToRcdType [(mkCons ds,[T.Int ds, T.Var ds (mkList ds)]), (mkNil ds, [])])

-- For constructors (used in Parser.y and here for lists)
typeListToType :: Variable -> [(Variable, [T.Type])] -> [(Variable, T.Type)]
typeListToType a = map $ second typeToFun -- map (\(x, ts) -> (x, typeToFun ts))
  -- Convert a list of types and a final type constructor to a type
 where
  typeToFun []       = T.Var (getSpan a) a
  typeToFun (t : ts) = T.Arrow (getSpan t) Un t (typeToFun ts)


typeListToRcdType :: [(Variable, [T.Type])] -> T.TypeMap
typeListToRcdType []             = Map.empty
typeListToRcdType ((c, us) : ts) =
  Map.insert c (T.Labelled (getSpan c) T.Record $ typesToMap 0 us) (typeListToRcdType ts)
  where typesToMap _ [] = Map.empty
        typesToMap n (t : ts) = Map.insert (mkVar (getSpan t) $ show n) t (typesToMap (n+1) ts)
