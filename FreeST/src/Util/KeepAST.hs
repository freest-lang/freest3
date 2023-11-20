{-# LANGUAGE FlexibleInstances #-}

module Util.KeepAST where


import           Syntax.Base
import qualified Syntax.Type       as T
import qualified Syntax.Kind       as K
import qualified Syntax.Expression as E
import qualified Data.Map.Strict   as Map

-- every instance of KeepAST keeps a copy of itself in its Span (if it has one)
-- keepAST is called recursively 
class KeepAST a where
    keepAST :: a -> a

defaultKeepAST :: Located a => a -> a
defaultKeepAST x = setSpan s{source=Just x} x
    where s = getSpan x

instance KeepAST T.TypeMap where
    keepAST tMap = Map.foldlWithKey' (\acc var t -> Map.insert (keepAST var) (keepAST t) acc) Map.empty tMap

instance KeepAST E.FieldMap where
    keepAST fMap = Map.foldlWithKey' (\acc var (vars, e) -> Map.insert (keepAST var) (map keepAST vars, keepAST e) acc) Map.empty fMap

instance KeepAST E.FieldList where
    keepAST fl = map (\(pats, e) -> (map keepAST pats, keepAST e)) fl

instance KeepAST E.Pattern where
    keepAST (E.PatVar v) = E.PatVar (keepAST v)
    keepAST (E.PatCons v pats) = E.PatCons (keepAST v) (map keepAST pats)

instance KeepAST Variable where
    keepAST v@Variable{} = defaultKeepAST v

instance KeepAST E.Exp where
    keepAST (E.Var s v) = defaultKeepAST $
        E.Var s (keepAST v)
    keepAST (E.Abs s m (Bind s' v t e)) = defaultKeepAST $
        E.Abs s m (Bind s' (keepAST v) (keepAST t) (keepAST e))
    keepAST (E.App s e1 e2) = defaultKeepAST $
        E.App s (keepAST e1) (keepAST e2)
    keepAST (E.Pair s e1 e2) = defaultKeepAST $
        E.Pair s (keepAST e1) (keepAST e2)
    keepAST (E.BinLet s v1 v2 e1 e2) = defaultKeepAST $
        E.BinLet s (keepAST v1) (keepAST v2) (keepAST e1) (keepAST e2)
    keepAST (E.Case s e fm) = defaultKeepAST $
        E.Case s (keepAST e) (keepAST fm)
    keepAST (E.CasePat s e fl) = defaultKeepAST $
        E.CasePat s (keepAST e) (keepAST fl)
    keepAST (E.TypeAbs s (Bind s' v k e)) = defaultKeepAST $
        E.TypeAbs s (Bind s' (keepAST v) (keepAST k) (keepAST e))
    keepAST (E.TypeApp s e t) = defaultKeepAST $
        E.TypeApp s (keepAST e) (keepAST t)
    keepAST (E.Cond s e1 e2 e3) = defaultKeepAST $
        E.Cond s (keepAST e1) (keepAST e2) (keepAST e3)
    keepAST (E.UnLet s v e1 e2) = defaultKeepAST $
        E.UnLet s (keepAST v) (keepAST e1) (keepAST e2)
    keepAST x = defaultKeepAST x


instance KeepAST T.Type where
    keepAST (T.Arrow s m t1 t2) = defaultKeepAST $
        T.Arrow s m (keepAST t1) (keepAST t2)
    keepAST (T.Labelled s srt tm) = defaultKeepAST $
        T.Labelled s srt (keepAST tm)
    keepAST (T.Semi s t1 t2) = defaultKeepAST $
        T.Semi s (keepAST t1) (keepAST t2)
    keepAST (T.Message s p t) = defaultKeepAST $
        T.Message s p (keepAST t)
    keepAST (T.Forall s (Bind s' v k t)) = defaultKeepAST $
        T.Forall s (Bind s' (keepAST v) (keepAST k) (keepAST t))
    keepAST (T.Rec s (Bind s' v k t)) = defaultKeepAST $
        T.Rec s (Bind s' (keepAST v) (keepAST k) (keepAST t))
    keepAST (T.Var s v) = defaultKeepAST $
        T.Var s (keepAST v)
    keepAST (T.Dualof s t) = defaultKeepAST $
        T.Dualof s (keepAST t)
    keepAST x = defaultKeepAST x

instance KeepAST K.Kind where
    keepAST = defaultKeepAST