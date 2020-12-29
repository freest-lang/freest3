module Utils.ShowDefault
  ( ShowWithDefault(..)
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


-- | Class show default

class ShowWithDefault a where
  showDefault :: TypeOpsEnv -> a -> String

-- | Show types, consulting the typename map

instance ShowWithDefault T.Type where
  showDefault tops t = show (showTypeDefault tops t)

showTypeDefault :: TypeOpsEnv -> T.Type -> T.Type
showTypeDefault tops (T.Semi p t u) =
  lookupPos tops p (T.Semi p (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (T.Rec p (K.Bind p' x k t)) = -- xs t
  lookupPos tops p (T.Rec p (K.Bind p' x k (showTypeDefault tops t)))
showTypeDefault tops (T.Forall p (K.Bind p' x k t)) = -- xs t
  lookupPos tops p (T.Forall p (K.Bind p' x k (showTypeDefault tops t)))
showTypeDefault tops (T.Fun p m t u) =
  lookupPos tops p (T.Fun p m (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (T.Pair p t u) =
  lookupPos tops p (T.Pair p (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (T.Datatype p m) =
  lookupPos tops p (T.Datatype p (Map.map (showTypeDefault tops) m))
showTypeDefault tops (T.Choice p pol m) =
  lookupPos tops p (T.Choice p pol (Map.map (showTypeDefault tops) m))
showTypeDefault tops t = Map.findWithDefault t (pos t) tops

lookupPos :: TypeOpsEnv -> Pos -> T.Type -> T.Type
lookupPos tops p defaultType = fromMaybe defaultType (tops Map.!? p)

instance ShowWithDefault Exp where
  showDefault tops e = show (showExpDefault tops e)

-- | Show expression, consulting the typename map

showExpDefault :: TypeOpsEnv -> Exp -> Exp
showExpDefault tops (Abs p1 (Bind p2 m b t e)) =
  Abs p1 (Bind p2 m b (showTypeDefault tops t) (showExpDefault tops e))
showExpDefault _    (TypeApp p x t) = TypeApp p x t
 --  TypeApp p x (map (showTypeDefault tops) ts)
showExpDefault tops (New     p t u) = New p (showTypeDefault tops t) u
showExpDefault _    e               = e
