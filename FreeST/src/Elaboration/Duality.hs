{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Elaboration.Duality
  ( Duality(..)
 -- , NegSubs(..)
  )
where

import           Syntax.Base                   (Bind(..), Variable, getSpan)
import qualified Data.Map                      as Map
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
-- import           Validation.Substitution
import           Util.KeepSrc

-- The dual of an object
class Duality t where
  dualof :: t -> t 

-- Lindley-Morris Duality, Yielding No New Negative Variables, Definition 34
-- https://arxiv.org/pdf/2004.01322.pdf

instance Duality T.Type where 
  -- Session Types
  dualof (T.Semi p t u) = T.Semi p (dualof t) (dualof u)
  dualof (T.Message p pol t) = T.Message p (dualof pol) t
  dualof (T.Labelled p (T.Choice v) m) =
    T.Labelled p (T.Choice $ dualof v) (Map.map dualof m)
  dualof (T.Var x)              = forceKeepSrc $ T.Dualof (getSpan x) $ T.Var x
  dualof (T.Dualof _ (T.Var x)) = T.Var x
  dualof (T.Dualof _ t) = dualof t
  dualof u@(T.Rec p (Bind p' a k t)) =
    let t' = subs (keepSrc $ T.Dualof p' (T.Var a)) a t in
      T.Rec p $ Bind p' a k (cosubs u a (dualof t'))
  -- Non session-types, Skip & End
  dualof t = t

instance Duality T.Polarity where
  dualof T.In  = T.Out
  dualof T.Out = T.In

instance Duality T.View where
  dualof T.Internal = T.External
  dualof T.External = T.Internal

-- CoVar subs, [t/co-x]u

class Cosubs t where
  cosubs :: T.Type -> Variable -> t -> t

instance Cosubs T.Type where
  -- Functional types
  cosubs t x (T.Message p pol t1) = T.Message p pol (cosubs t x t1)
  cosubs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (cosubs t x t1) (cosubs t x t2)
  -- Session types
  cosubs t x (T.Semi   p t1 t2  ) = T.Semi p (cosubs t x t1) (cosubs t x t2)
  cosubs t x (T.Labelled p s  m   ) = T.Labelled p s (Map.map (cosubs t x) m)
    -- Polymorphism and recursion
  cosubs t x (T.Rec    p b      ) = T.Rec p (cosubs t x b)
  cosubs t x (T.Forall p b      ) = T.Forall p (cosubs t x b)
  cosubs t x u@(T.Dualof _ (T.Var y))
    | y == x = t
    | otherwise = u
  cosubs _ _ t            = t


instance Cosubs t => Cosubs (Bind K.Kind t) where
  cosubs t x (Bind p y k u) = Bind p y k (cosubs t x u)


-- Duplicated. See Validation.Substitution

class Subs t x u where
  subs :: t -> x -> u -> u
  
instance Subs T.Type Variable T.Type where
  -- Labelled
  subs t x (T.Labelled p s m   ) = T.Labelled p s (Map.map (subs t x) m)
  -- Functional types
  subs t x (T.Message p pol t1) = T.Message p pol (subs t x t1)
  subs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (subs t x t1) (subs t x t2)
  -- Session types
  subs t x (T.Semi   p t1 t2  ) = T.Semi p (subs t x t1) (subs t x t2)
  -- Polymorphism and recursion
  subs t x (T.Rec    p b      ) = T.Rec p (subs t x b)
  subs t x (T.Forall p b      ) = T.Forall p (subs t x b)
  subs t x u@(T.Var y)
    | y == x    = t
    | otherwise = u
  subs (T.Var t) x u@(T.Dualof p (T.Var y))
    | y == x    = T.Dualof p $ T.Var t
    | otherwise = u
  subs t x u@(T.Dualof p (T.Var y))
    | y == x    = dualof t
    | otherwise = u
  subs _ _ t            = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Validation.Substitution.subs" t


instance (Subs T.Type Variable t) => Subs T.Type Variable (Bind k t) where
  subs t x (Bind p y k u) = Bind p y k (subs t x u)
