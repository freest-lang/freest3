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
  replace (  T.Labelled p s m ) = T.Labelled p s <$> replace m
  replace (  T.Message p pol t) = T.Message p pol <$> replace t
  replace (  T.Arrow p m t1 t2  ) = T.Arrow p m <$> replace t1 <*> replace t2
  replace (  T.Semi   p t1  t2) = T.Semi p <$> replace t1 <*> replace t2
  replace (  T.Forall p kb    ) = T.Forall p <$> replace kb
  replace (  T.Rec    p kb    ) = T.Rec p <$> replace kb
  replace n@(T.Var    p tname ) = getFromTypes tname >>= \case
    Just t  -> pure (changePos p (snd t))
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


-- | Changing positions
-- Change position of a given type with a given position
changePos :: Span -> T.Type -> T.Type
changePos p (T.Int  s          ) = setSrc (source s) $ T.Int p
changePos p (T.Char s          ) = setSrc (source s) $ T.Char p
changePos p (T.Arrow s pol t u ) = setSrc (source s) $ T.Arrow p pol (changePos p t) (changePos p u)
-- Datatype
-- Skip
changePos p (T.Semi     s t   u) = setSrc (source s) $ T.Semi p t u
changePos p (T.Message  s pol b) = setSrc (source s) $ T.Message p pol b
changePos p (T.Labelled s srt m) = setSrc (source s) $ T.Labelled p srt m
changePos p (T.Rec      s xs   ) = setSrc (source s) $ T.Rec p xs
changePos p (T.Forall   s xs   ) = setSrc (source s) $ T.Forall p xs
-- TypeVar
changePos _ t                   = t
