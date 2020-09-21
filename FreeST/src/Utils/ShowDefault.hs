module Utils.ShowDefault
  ( ShowWithDefault(..)
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Syntax.Base
import           Syntax.Expressions
import           Syntax.Show                    ( )
import           Syntax.Types                   ( Type(..)
                                                , TypeOpsEnv
                                                )

-- | Class show default

class ShowWithDefault a where
  showDefault :: TypeOpsEnv -> a -> String

-- | Show types, consulting the typename map

instance ShowWithDefault Type where
  showDefault tops t = show (showTypeDefault tops t)

showTypeDefault :: TypeOpsEnv -> Type -> Type
showTypeDefault tops (Semi p t u) =
  lookupPos tops p (Semi p (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (Rec p xs t) =
  lookupPos tops p (Rec p xs (showTypeDefault tops t))
showTypeDefault tops (Fun p m t u) =
  lookupPos tops p (Fun p m (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (PairType p t u) = lookupPos
  tops
  p
  (PairType p (showTypeDefault tops t) (showTypeDefault tops u))
showTypeDefault tops (Datatype p m) =
  lookupPos tops p (Datatype p (Map.map (showTypeDefault tops) m))
showTypeDefault tops (Choice p pol m) =
  lookupPos tops p (Choice p pol (Map.map (showTypeDefault tops) m))
showTypeDefault tops t = Map.findWithDefault t (position t) tops

lookupPos :: TypeOpsEnv -> Pos -> Type -> Type
lookupPos tops p defaultType = fromMaybe defaultType (tops Map.!? p)

instance ShowWithDefault Expression where
  showDefault tops e = show (showExpDefault tops e)

-- | Show expression, consulting the typename map

showExpDefault :: TypeOpsEnv -> Expression -> Expression
showExpDefault tops (Lambda p m b t e) =
  Lambda p m b (showTypeDefault tops t) (showExpDefault tops e)
showExpDefault tops (TypeApp p x ts) =
  TypeApp p x (map (showTypeDefault tops) ts)
showExpDefault tops (New p t u) = New p (showTypeDefault tops t) u
showExpDefault _    e           = e
