{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, LambdaCase #-}
module Elaboration.Elaborate where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
-- import           Util.FreestState
import           Elaboration.Phase
import           Util.State.State

-- | Elaboration: Substitutions over Type, Exp, TypeMap, FieldMap, and Binds

class Elaboration t where
  elaborate :: t -> ElabState t

instance Elaboration T.Type where
  elaborate (  T.Labelled p s m ) = T.Labelled p s <$> elaborate m
  elaborate (  T.Message p pol t) = T.Message p pol <$> elaborate t
  elaborate (  T.Arrow p m t1 t2  ) = T.Arrow p m <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Semi   p t1  t2) = T.Semi p <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Forall p kb    ) = T.Forall p <$> elaborate kb
  elaborate (  T.Rec    p kb    ) = T.Rec p <$> elaborate kb
  elaborate n@(T.Var    p tname ) = getFromTypes tname >>= \case
    Just t  -> addTypeName p n >> pure (changePos p (snd t))
    Nothing -> pure n
  elaborate (T.Dualof p t) = T.Dualof p <$> elaborate t
  elaborate t              = pure t

instance Elaboration T.TypeMap where
  elaborate = mapM elaborate

instance Elaboration a => Elaboration (Bind K.Kind a) where
  elaborate (Bind p x k a) = Bind p x k <$> elaborate a

-- instance Elaboration (Bind K.Kind Exp) where
--   elaborate (Bind p x k e) = Bind p x k <$> elaborate e

instance Elaboration (Bind T.Type E.Exp) where
  elaborate (Bind p x t e) = Bind p x <$> elaborate t <*> elaborate e

-- Substitute expressions

instance Elaboration E.Exp where
  elaborate (E.Abs p m b   ) = E.Abs p m <$> elaborate b
  elaborate (E.App  p e1 e2) = E.App p <$> elaborate e1 <*> elaborate e2
  elaborate (E.Pair p e1 e2) = E.Pair p <$> elaborate e1 <*> elaborate e2
  elaborate (E.BinLet p x y e1 e2) =
    E.BinLet p x y <$> elaborate e1 <*> elaborate e2
  elaborate (E.Case p e m) = E.Case p <$> elaborate e <*> elaborate m
  elaborate (E.TypeApp p e t  ) = E.TypeApp p <$> elaborate e <*> elaborate t
  elaborate (E.TypeAbs p b    ) = E.TypeAbs p <$> elaborate b
  elaborate (E.UnLet p x e1 e2) = E.UnLet p x <$> elaborate e1 <*> elaborate e2
  elaborate e                 = return e

instance Elaboration E.FieldMap where
  elaborate = mapM (\(ps, e) -> (ps, ) <$> elaborate e)


-- | Changing positions

-- Change position of a given type with a given position
changePos :: Span -> T.Type -> T.Type
changePos p (T.Int  _         ) = T.Int p
changePos p (T.Float _        ) = T.Float p
changePos p (T.Char _         ) = T.Char p
changePos p (T.Arrow _ pol t u) = T.Arrow p pol (changePos p t) (changePos p u)
-- Datatype
-- Skip
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Labelled _ s   m) = T.Labelled p s m
changePos p (T.Rec     _ xs   ) = T.Rec p xs
changePos p (T.Forall  _ xs   ) = T.Forall p xs
-- TypeVar
changePos _ t                   = t
