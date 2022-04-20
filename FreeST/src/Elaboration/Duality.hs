module Elaboration.Duality where

import           Syntax.Base                   (Bind(..))
import qualified Data.Map                      as Map
import qualified Syntax.Type                   as T
import           Util.FreestState
import           Util.Error
import           Validation.Substitution



-- Calculates the dual of a session type
class Duality t where
  dualof :: t -> t 

-- Lindley-Morris Duality, Polished, Definition 31
-- https://arxiv.org/pdf/2004.01322.pdf
instance Duality T.Type where 
  -- Session Types
  dualof (T.Semi p t u) = T.Semi p (dualof t) (dualof u)
  dualof (T.Message p pol t) = T.Message p (dualof pol) t
  -- dualof (T.Message p pol t) = T.Message p (dual pol) (dualof t)
 -- dualof (T.Choice p pol m) = T.Choice p (dual pol) (Map.map dualof m)
  dualof (T.Almanac p (T.Choice v) m) =
    T.Almanac p (T.Choice $ dualof v) (Map.map dualof m)
  dualof (T.Rec p (Bind p' a k t)) =
    T.Rec p (Bind p' a k (dualof (subs (T.CoVar p' a) a t)))
  -- T.Rec p (dualBind  b)
  --   where dualBind (K.Bind p a k t) = K.Bind p a k (dualof t)
  dualof (T.Var p x) = T.CoVar p x
  dualof (T.CoVar p x) = T.Var p x
  dualof (T.Dualof _ t) = dualof t
  -- Non session-types & Skip
  dualof t = t

instance Duality T.Polarity where
  dualof T.In  = T.Out
  dualof T.Out = T.In

instance Duality T.View where
  dualof T.Internal = T.External
  dualof T.External = T.Internal
