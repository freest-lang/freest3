{-# LANGUAGE FlexibleInstances #-}
module Util.GetTOps
  ( DefaultTypeOp(..)
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Parse.Unparser                 ( )
import           Syntax.Base
import           Syntax.Expression
import           Syntax.Program
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K

-- | Class to get the type operators back to their original locations

class DefaultTypeOp a where
  getDefault :: TypeOpsEnv -> a -> a

instance DefaultTypeOp T.Type where
  getDefault m (T.Arrow p mu t u) =
    lookupPos m p $ T.Arrow p mu (getDefault m t) (getDefault m u)
  getDefault m (T.Pair p t u) =
    lookupPos m p $ T.Pair p (getDefault m t) (getDefault m u)
  getDefault m (T.Datatype p sm) =
    lookupPos m p $ T.Datatype p $ getDefault m sm
  getDefault m (T.Semi p t u) =
    lookupPos m p $ T.Semi p (getDefault m t) (getDefault m u)
  getDefault m (T.Message p pol t) =
    lookupPos m p $ T.Message p pol $ getDefault m t
  getDefault m (T.Choice p pol cm) =
    lookupPos m p $ T.Choice p pol $ getDefault m cm
  getDefault m (T.Forall p b) = lookupPos m p $ T.Forall p $ getDefault m b
  getDefault m (T.Rec    p b) = lookupPos m p $ T.Rec p $ getDefault m b
  getDefault _ t              = t

instance DefaultTypeOp Exp where
  getDefault m (Abs p b     ) = Abs p $ getDefault m b
  getDefault m (App  p e1 e2) = App p (getDefault m e1) (getDefault m e2)
  getDefault m (Pair p e1 e2) = Pair p (getDefault m e1) (getDefault m e2)
  getDefault m (BinLet p x y e1 e2) =
    BinLet p x y (getDefault m e1) (getDefault m e2)
  getDefault m (Case p e fm  ) = Case p (getDefault m e) (getDefault m fm)
  getDefault m (TypeAbs p b  ) = TypeAbs p $ getDefault m b
  getDefault m (TypeApp p e t) = TypeApp p (getDefault m e) (getDefault m t)
  getDefault m (Cond p e1 e2 e3) =
    Cond p (getDefault m e1) (getDefault m e2) (getDefault m e3)
  getDefault m (UnLet p x e1 e2) =
    UnLet p x (getDefault m e1) (getDefault m e2)
  getDefault m (New   p t u ) = New p (getDefault m t) (getDefault m u)
  getDefault _ e              = e

instance DefaultTypeOp (K.Bind Exp) where
  getDefault m (K.Bind p x k e) = K.Bind p x k $ getDefault m e

instance DefaultTypeOp Bind where
  getDefault m (Bind p mul x k t) = Bind p mul x k $ getDefault m t

instance DefaultTypeOp FieldMap where
  getDefault m = Map.map (\(x, y) -> (x, getDefault m y))

instance DefaultTypeOp (K.Bind T.Type) where
  getDefault m (K.Bind p x k t) = K.Bind p x k $ getDefault m t

instance DefaultTypeOp T.TypeMap where
  getDefault m = Map.map (getDefault m)

lookupPos :: TypeOpsEnv -> Pos -> T.Type -> T.Type
lookupPos tops p defaultType = fromMaybe defaultType (tops Map.!? p)
