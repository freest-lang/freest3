{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}

module Util.KeepSrc where

import qualified Data.Map as Map
import           Parse.Unparser ( Unparse, unparse )
import           Syntax.Base ( Span(source), Located(..), Variable(..), Bind(..) )
import           Syntax.Type as T
import           Syntax.Expression as E
import           Syntax.Program ( VarEnv, TypeEnv, Prog )

class KeepSrc a where
    keepSrc :: a -> a

instance KeepSrc E.Exp where
    keepSrc (E.Var s v) = defaultKeepSrc $
        E.Var s (keepSrc v)
    keepSrc (E.App s e1 e2) = defaultKeepSrc $
        E.App s (keepSrc e1) (keepSrc e2)
    keepSrc (E.Pair s e1 e2) = defaultKeepSrc $
        E.Pair s (keepSrc e1) (keepSrc e2)
    keepSrc (E.BinLet s v1 v2 e1 e2) = 
        E.BinLet s (keepSrc v1) (keepSrc v2) (keepSrc e1) (keepSrc e2)
    keepSrc (E.Case s e fm) = defaultKeepSrc $
        E.Case s (keepSrc e) (keepSrc fm)
    keepSrc (E.CasePat s e fl) = defaultKeepSrc $
        E.CasePat s (keepSrc e) (keepSrc fl)
    keepSrc (E.TypeAbs s (Bind s' v k e)) = defaultKeepSrc $
        E.TypeAbs s (Bind s' (keepSrc v) k (keepSrc e))
    keepSrc (E.TypeApp s e t) = defaultKeepSrc $
        E.TypeApp s (keepSrc e) (keepSrc t)
    keepSrc (E.Cond s e1 e2 e3) = defaultKeepSrc $
        E.Cond s (keepSrc e1) (keepSrc e2) (keepSrc e3)
    keepSrc (E.UnLet s v e1 e2) = defaultKeepSrc $
        E.UnLet s (keepSrc v) (keepSrc e1) (keepSrc e2)
    keepSrc x = defaultKeepSrc x


instance KeepSrc T.Type where
    keepSrc (T.Arrow s m t1 t2) = defaultKeepSrc $
        T.Arrow s m (keepSrc t1) (keepSrc t2)
    keepSrc (T.Labelled s T.Record tm) = defaultKeepSrc $
        T.Labelled s T.Record (defaultKeepSrcMapKeys tm)
    keepSrc (T.Labelled s srt tm) = --defaultKeepSrc $
        T.Labelled s srt (defaultKeepSrcMapKeys tm)
    keepSrc (T.Semi s t1 t2) = defaultKeepSrc $
        T.Semi s (keepSrc t1) (keepSrc t2)
    keepSrc (T.Message s p t) = defaultKeepSrc $
        T.Message s p (keepSrc t)
    keepSrc (T.Forall s (Bind s' v k t)) = defaultKeepSrc $
        T.Forall s (Bind s' (keepSrc v) k (keepSrc t))
    keepSrc (T.Rec s (Bind s' v k t)) = defaultKeepSrc $
        T.Rec s (Bind s' (keepSrc v) k (keepSrc t))
    keepSrc (T.Var s v) = defaultKeepSrc $
        T.Var s (keepSrc v)
    keepSrc (T.Dualof s t) = defaultKeepSrc $
        T.Dualof s (keepSrc t)
    keepSrc x = defaultKeepSrc x

instance KeepSrc Variable where
    keepSrc (Variable s v) = Variable s{source=v} v

instance KeepSrc E.FieldMap where
    keepSrc = Map.fromList . map (\(k, (vars, e)) -> (keepSrc k, (map keepSrc vars, keepSrc e))) . Map.toList

instance KeepSrc E.FieldList where
    keepSrc = map (\(vars, e) -> (map keepSrc vars, keepSrc e))

instance KeepSrc Pattern where
    keepSrc (PatVar v) = PatVar $ keepSrc v
    keepSrc (PatCons v pats) = PatCons (keepSrc v) $ map keepSrc pats


instance KeepSrc VarEnv where
    keepSrc = defaultKeepSrcMapKeys

instance KeepSrc TypeEnv where
    keepSrc = Map.fromList . map (\(v, (k, t)) -> (keepSrc v, (k, keepSrc t))) . Map.toList

instance KeepSrc Prog where
    keepSrc = defaultKeepSrcMapKeys

defaultKeepSrc :: (Unparse a, Located a) => a -> a
defaultKeepSrc x = setSrc (snd $ unparse x) x

-- defaultKeepSrcMap :: Map a k -> Map a k
-- defaultKeepSrcMap m = Map.map 

defaultKeepSrcMapKeys :: (Ord a, KeepSrc a, KeepSrc k) => Map.Map a k -> Map.Map a k
defaultKeepSrcMapKeys = 
    Map.fromList . map (\(k, v) -> (keepSrc k, keepSrc v)) . Map.toList





