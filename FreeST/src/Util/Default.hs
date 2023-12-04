{-# LANGUAGE FlexibleInstances #-}

module Util.Default where

import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Type as T
import qualified Syntax.Kind as K
import           Util.StoreSource

-- Default for the various syntactic categories

class Default t where
  omission :: Span a -> t

instance Default (Bind T.Type E.Exp) where
  omission p = Bind (clearSource p) (omission (clearSource p)) (T.unit (clearSource p)) (E.Unit (clearSource p))

instance (Default a) => Default (Bind K.Kind a) where
  omission p = Bind p' (omission p') (omission p') (omission p')
    where p' = clearSource p

instance Default Variable where
  omission p = mkVar p "omission"

-- The kind of conventional (non linear, non session) functional programming
-- languages' types (Alternative: the kind that sits at the top of the
-- hierarchy)
instance Default K.Kind where
  omission _ = K.ut defaultSpan

instance Default T.Type where
  omission s = T.Int s'{source = Just $ T.Int s'}
    where s' = clearSource s