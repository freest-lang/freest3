{-# LANGUAGE FlexibleInstances #-}

module Util.StoreSource where


import           Syntax.Base
import qualified Syntax.Type       as T
import qualified Syntax.Kind       as K
import qualified Syntax.Expression as E
import qualified Data.Map.Strict   as Map

-- every instance of Storable keeps a copy of itself in its Span (if it has one)
-- storeSource is called recursively 
class Storable a where
    storeSource :: a -> a

defaultKeepAST :: Located a => a -> a
defaultKeepAST x = setSpan s{source=Just x} x
    where s = getSpan x

instance Storable T.TypeMap where
    storeSource tMap = Map.foldlWithKey' (\acc var t -> Map.insert (storeSource var) (storeSource t) acc) Map.empty tMap

instance Storable E.FieldMap where
    storeSource fMap = Map.foldlWithKey' (\acc var (vars, e) -> Map.insert (storeSource var) (map storeSource vars, storeSource e) acc) Map.empty fMap

instance Storable E.FieldList where
    storeSource fl = map (\(pats, e) -> (map storeSource pats, storeSource e)) fl

instance Storable E.Pattern where
    storeSource (E.PatVar v) = E.PatVar (storeSource v)
    storeSource (E.PatCons v pats) = E.PatCons (storeSource v) (map storeSource pats)

instance Storable Variable where
    storeSource v@Variable{} = defaultKeepAST v

instance Storable E.Exp where
    storeSource (E.Var s v) = defaultKeepAST $
        E.Var s (storeSource v)
    storeSource (E.Abs s m (Bind s' v t e)) = defaultKeepAST $
        E.Abs s m (Bind s' (storeSource v) (storeSource t) (storeSource e))
    storeSource (E.App s e1 e2) = defaultKeepAST $
        E.App s (storeSource e1) (storeSource e2)
    storeSource (E.Pair s e1 e2) = defaultKeepAST $
        E.Pair s (storeSource e1) (storeSource e2)
    storeSource (E.BinLet s v1 v2 e1 e2) = defaultKeepAST $
        E.BinLet s (storeSource v1) (storeSource v2) (storeSource e1) (storeSource e2)
    storeSource (E.Case s e fm) = defaultKeepAST $
        E.Case s (storeSource e) (storeSource fm)
    storeSource (E.CasePat s e fl) = defaultKeepAST $
        E.CasePat s (storeSource e) (storeSource fl)
    storeSource (E.TypeAbs s (Bind s' v k e)) = defaultKeepAST $
        E.TypeAbs s (Bind s' (storeSource v) (storeSource k) (storeSource e))
    storeSource (E.TypeApp s e t) = defaultKeepAST $
        E.TypeApp s (storeSource e) (storeSource t)
    storeSource (E.Cond s e1 e2 e3) = defaultKeepAST $
        E.Cond s (storeSource e1) (storeSource e2) (storeSource e3)
    storeSource (E.UnLet s v e1 e2) = defaultKeepAST $
        E.UnLet s (storeSource v) (storeSource e1) (storeSource e2)
    storeSource x = defaultKeepAST x


instance Storable T.Type where
    storeSource (T.Arrow s m t1 t2) = defaultKeepAST $
        T.Arrow s m (storeSource t1) (storeSource t2)
    storeSource (T.Labelled s srt tm) = defaultKeepAST $
        T.Labelled s srt (storeSource tm)
    storeSource (T.Semi s t1 t2) = defaultKeepAST $
        T.Semi s (storeSource t1) (storeSource t2)
    storeSource (T.Message s p t) = defaultKeepAST $
        T.Message s p (storeSource t)
    storeSource (T.Forall s (Bind s' v k t)) = defaultKeepAST $
        T.Forall s (Bind s' (storeSource v) (storeSource k) (storeSource t))
    storeSource (T.Rec s (Bind s' v k t)) = defaultKeepAST $
        T.Rec s (Bind s' (storeSource v) (storeSource k) (storeSource t))
    storeSource (T.Var s v) = defaultKeepAST $
        T.Var s (storeSource v)
    storeSource (T.Dualof s t) = defaultKeepAST $
        T.Dualof s (storeSource t)
    storeSource x = defaultKeepAST x

instance Storable K.Kind where
    storeSource = defaultKeepAST