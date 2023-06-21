{-# LANGUAGE TypeFamilies #-}

module Syntax.AST where

import           Syntax.Base
import qualified Syntax.Kind as K
import qualified Syntax.Type as T

import qualified Data.Map as Map

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
  { types       = Map.empty
  , signatures  = Map.empty
  , definitions = Map.empty
  }

addSignature :: Variable -> T.Type -> AST a -> AST a
addSignature f t ast = ast{signatures = Map.insert f t (signatures ast)} 

addType :: Variable -> K.Kind -> T.Type -> AST a -> AST a
addType x k t ast = ast{types = Map.insert x (k,t) (types ast)}

-- Generic add definition, depends on the instantiation of the type family XDef.
-- In some phases we may need a more specific implementation
addDefinition :: Variable -> XDef a -> AST a -> AST a
addDefinition x d ast = ast{definitions = Map.insert x d (definitions ast)}
