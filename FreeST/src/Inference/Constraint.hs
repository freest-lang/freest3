module Inference.Constraint where

import           Syntax.Base
import qualified Syntax.Kind as K

data Constraint =
    KindC K.Kind K.Kind
  | PKMeetC Variable [K.Kind]
  | PKJoinC Variable [K.Kind]
  | MultC Variable [K.Kind]
  deriving Eq

-- instance Show Constraint where
--   show (KindC k1 k2) = show k1 ++ " <: " ++ show k2
--   show (MultC v ks) = show v ++ " = ⊔" ++ show ks
--   show (PKMeetC v ks) = show v ++ " = " ++ " ⊓" ++ show ks
--   show (PKJoinC v ks) = show v ++ " = " ++ " ⊔" ++ show ks

fromPKToVar :: K.PreKind -> Variable
fromPKToVar (K.PKVar x) = x
fromPKToVar _ = undefined

fromMultToVar :: Multiplicity -> Variable
fromMultToVar (MultVar x) = x 
fromMultToVar _ = undefined 
