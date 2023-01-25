{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Elaboration.Duality
  ( Duality(..)
 -- , NegSubs(..)
  )
where

import           Syntax.Base                   (Bind(..), Variable)
import qualified Data.Map                      as Map
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
-- import           Validation.Substitution
import Debug.Trace


-- The dual of an object
class Duality t where
  dualof :: t -> t 

-- Lindley-Morris Duality, Polished, Definition 31
-- https://arxiv.org/pdf/2004.01322.pdf

-- dualLMN((μX.S)) = μX.((dualLMN(S[~X/X]))[μX.S/X])

instance Duality T.Type where 
  -- Session Types
  dualof (T.Semi p t u) = T.Semi p (dualof t) (dualof u)
  dualof (T.Message p pol t) = T.Message p (dualof pol) t
 -- dualof (T.Choice p pol m) = T.Choice p (dual pol) (Map.map dualof m)
  dualof (T.Almanac p (T.Choice v) m) =
    T.Almanac p (T.Choice $ dualof v) (Map.map dualof m)
  dualof (T.Var p x)              = T.Dualof p $ T.Var p x
  dualof (T.Dualof _ (T.Var p x)) = T.Var p x
  dualof (T.Dualof _ t) = dualof t
--   dualof u@(T.Rec p (Bind p' a k t)) =
-- --    subs u a (T.Rec p (Bind p' a k (dualof t)))
--     T.Rec p (Bind p' a k (subs (T.Dualof p' $ T.Var p' a) a (dualof t)))
-- --    T.Rec p (Bind p' a k (dualof (subs (T.Dualof p' $ T.Var p' a) a t)))
  dualof u@(T.Rec p (Bind p' a k t)) =
    -- dualLMN((μX.S)) = μX.((dualLMN(S[~X/X]))[μX.S/~X])
    -- let d = dualof t in
    -- let r = negSubs (T.Dualof p' (T.Var p' a)) a d in
    --    T.Rec p $ Bind p' a k $ cosubs u a r
    let t' = subs (T.Dualof p' (T.Var p' a)) a t in
    let d = dualof t' in
       T.Rec p $ Bind p' a k (cosubs u a d)
    
    
  -- Non session-types, Skip & End
  dualof t = t

instance Duality T.Polarity where
  dualof T.In  = T.Out
  dualof T.Out = T.In

instance Duality T.View where
  dualof T.Internal = T.External
  dualof T.External = T.Internal



-- dualLMP(μX.S) = μX.dualLMP(S[~X/X])


-- -- [t/x]u, substitute t for for every occurrence of x in u
-- class NegSubs t x u where
--   negSubs :: t -> x -> u -> u


-- -- def 27
-- instance NegSubs T.Type Variable T.Type where
--  negSubs t x (T.Almanac p s m   ) = T.Almanac p s (Map.map (negSubs t x) m)
--  negSubs t x (T.Message p pol t1) = T.Message p pol (negSubs t x t1)
--  negSubs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (negSubs t x t1) (negSubs t x t2)
--  negSubs t x (T.Semi   p t1 t2  ) = T.Semi p (negSubs t x t1) (negSubs t x t2)
--  negSubs t x (T.Rec    p b      ) = T.Rec p (negSubs t x b)
--  negSubs t x (T.Forall p b      ) = T.Forall p (negSubs t x b)
--  negSubs t x u@(T.Var _ y)
--    | y == x    = t
--    | otherwise = u
--  negSubs (T.Dualof _ (T.Var _ z)) x u@(T.Dualof p2 (T.Var _ y))
--    | z == y = T.Var p2 x
--    | otherwise = u
--  negSubs _ _ t            = t

-- instance (NegSubs T.Type Variable t) => NegSubs T.Type Variable (Bind k t) where
--   negSubs t x (Bind p y k u) = Bind p y k (negSubs t x u)

-- CoVar subs, [t/co-x]u

class Cosubs t where
  cosubs :: T.Type -> Variable -> t -> t

instance Cosubs T.Type where
  -- Functional types
  cosubs t x (T.Message p pol t1) = T.Message p pol (cosubs t x t1)
  cosubs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (cosubs t x t1) (cosubs t x t2)
  -- Session types
  cosubs t x (T.Semi   p t1 t2  ) = T.Semi p (cosubs t x t1) (cosubs t x t2)
  cosubs t x (T.Almanac p s  m   ) = T.Almanac p s (Map.map (cosubs t x) m)
    -- Polymorphism and recursion
  cosubs t x (T.Rec    p b      ) = T.Rec p (cosubs t x b)
  cosubs t x (T.Forall p b      ) = T.Forall p (cosubs t x b)
  cosubs t x u@(T.Dualof _ (T.Var _ y))
    | y == x = t
    | otherwise = u
  cosubs _ _ t            = t


instance Cosubs t => Cosubs (Bind K.Kind t) where
  cosubs t x (Bind p y k u) = Bind p y k (cosubs t x u)


class Subs t x u where
  subs :: t -> x -> u -> u
  
instance Subs T.Type Variable T.Type where
  -- Almanac
  subs t x (T.Almanac p s m   ) = T.Almanac p s (Map.map (subs t x) m)
  -- Functional types
  subs t x (T.Message p pol t1) = T.Message p pol (subs t x t1)
  subs t x (T.Arrow p m t1 t2 ) = T.Arrow p m (subs t x t1) (subs t x t2)
  -- Session types
  subs t x (T.Semi   p t1 t2  ) = T.Semi p (subs t x t1) (subs t x t2)
  -- Polymorphism and recursion
  subs t x (T.Rec    p b      ) = T.Rec p (subs t x b)
  subs t x (T.Forall p b      ) = T.Forall p (subs t x b)
  subs t x u@(T.Var _ y)
    | y == x    = t
    | otherwise = u
  subs (T.Var _ t) x u@(T.Dualof p (T.Var p' y))
    | y == x    = T.Dualof p $ T.Var p' t
    | otherwise = u
  subs t x u@(T.Dualof p (T.Var p' y))
    | y == x    = dualof t
       -- let tmp = dualof t in
       -- trace ("subs [t/x] (dual u): " ++ show t ++ " || " ++ show x ++ " || " ++ show y ++
       --        "\nRESULT:              " ++ show tmp ++ "\n") 
       --  tmp
    | otherwise = u
  subs _ _ t            = t
  -- Can't issue this error because we use
  -- this function during the elaboration of dualofs
  --  subs _ _ t@T.Dualof{} = internalError "Validation.Substitution.subs" t


instance (Subs T.Type Variable t) => Subs T.Type Variable (Bind k t) where
  subs t x (Bind p y k u) = Bind p y k (subs t x u)
