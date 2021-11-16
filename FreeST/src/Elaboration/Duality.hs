module Elaboration.Duality where

import qualified Data.Map                      as Map
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T


-- Calculates the dual of a session type
class Duality t where
  dualof :: t -> t

instance Duality T.Type where 
  -- Session Types
  dualof (T.Semi    p t   u) = T.Semi p (dualof t) (dualof u)
  dualof (T.Message p pol t) = T.Message p (dualof pol) (dualof t)
  dualof (T.Choice p pol m) = T.Choice p (dualof pol) (Map.map dualof m)
  dualof (T.Rec p b) = T.Rec p (dualBind  b)
    where dualBind (K.Bind p a k t) = K.Bind p a k (dualof t) 
  dualof (T.Dualof _ t) = dualof t
  -- Non session-types & Skip
  dualof t = t

instance Duality T.Polarity where
  dualof T.In  = T.Out
  dualof T.Out = T.In

instance Duality T.View where
  dualof T.Internal = T.External
  dualof T.External = T.Internal
