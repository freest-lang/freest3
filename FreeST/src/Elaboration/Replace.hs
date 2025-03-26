{-# LANGUAGE FlexibleInstances, TupleSections, LambdaCase #-}
module Elaboration.Replace where

import           Elaboration.Phase
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Type as T
import           Util.State

-- | Replace: Substitutions over Type, Exp, TypeMap, FieldMap, and Binds

class Replace t where
  replace :: t -> ElabState t

instance Replace T.Type where
  replace (  T.Labelled p s l m ) = T.Labelled p s l <$> replace m
  replace (  T.Message p l pol t) = T.Message p l pol <$> replace t
  replace (  T.Arrow p m l1 l2 t1 t2  ) = T.Arrow p m l1 l2 <$> replace t1 <*> replace t2
  replace (  T.Semi   p t1  t2) = T.Semi p <$> replace t1 <*> replace t2
  replace (  T.Forall p kb    ) = T.Forall p <$> replace kb
  replace (  T.Rec    p kb    ) = T.Rec p <$> replace kb
  replace n@(T.Var    p tname ) = getFromTypes tname >>= \case
    Just t  -> addTypeName p n >> pure (changePos p (snd t))
    Nothing -> pure n
  replace (T.Dualof p t) = T.Dualof p <$> replace t
  replace t              = pure t

instance Replace T.TypeMap where
  replace = mapM replace

instance Replace a => Replace (Bind K.Kind a) where
  replace (Bind p x k a) = Bind p x k <$> replace a

-- instance Replace (Bind K.Kind Exp) where
--   replace (Bind p x k e) = Bind p x k <$> replace e

instance Replace (Bind T.Type E.Exp) where
  replace (Bind p x t e) = Bind p x <$> replace t <*> replace e

-- Substitute expressions

instance Replace E.Exp where
  replace (E.Abs p m b   ) = E.Abs p m <$> replace b
  replace (E.App  p e1 e2) = E.App p <$> replace e1 <*> replace e2
  replace (E.Pair p e1 e2) = E.Pair p <$> replace e1 <*> replace e2
  replace (E.BinLet p x y e1 e2) =
    E.BinLet p x y <$> replace e1 <*> replace e2
  replace (E.Case p e m) = E.Case p <$> replace e <*> replace m
  replace (E.TypeApp p e t  ) = E.TypeApp p <$> replace e <*> replace t
  replace (E.TypeAbs p b    ) = E.TypeAbs p <$> replace b
  replace (E.UnLet p x e1 e2) = E.UnLet p x <$> replace e1 <*> replace e2
  replace e                 = return e

instance Replace E.FieldMap where
  replace = mapM (\(ps, e) -> (ps, ) <$> replace e)

-- Replace the span of a type with a given span
changePos :: Span -> T.Type -> T.Type
  -- Functional Types
changePos s (T.Int _          ) = T.Int s
changePos s (T.Float _        ) = T.Float s
changePos s (T.Char _         ) = T.Char s
changePos s (T.String _       ) = T.String s
changePos s (T.Arrow _ p l1 l2 t u  ) = T.Arrow s p l1 l2 t u
-- changePos s (T.Arrow _ pol t u) = T.Arrow s pol (changePos s t) (changePos s u)
changePos s (T.Labelled _ st l m) = T.Labelled s st l m
  -- Session Types
changePos s (T.Skip _         ) = T.Skip s
changePos s (T.End _ p        ) = T.End s p
changePos s (T.Semi _ t u     ) = T.Semi s t u
changePos s (T.Message _ l p t  ) = T.Message s l p t
  -- Polymorphism and recursive types
changePos s (T.Forall _ b     ) = T.Forall s b
changePos s (T.Rec _ b        ) = T.Rec s b
changePos s (T.Var _ v        ) = T.Var s v
  -- Type operators
changePos s (T.Dualof _ t     ) = T.Dualof s t
-- changePos _ t                   = t
