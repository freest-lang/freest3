{-# LANGUAGE FlexibleInstances, TupleSections  #-}

module Util.DeBruijn where

import           Syntax.Base
import qualified Syntax.Expression  as E
import qualified Syntax.Type        as T
import           Data.List                 (elemIndex)
import qualified Data.Map.Strict    as Map
import           Util.FreestState          (FreestState, tMapM, addError)
import           Util.Error                (ErrorType(VarOrConsNotInScope, TypeVarNotInScope))
import           Data.Functor              (($>))
import           Util.PreludeLoader        (prelude)


-- Indices
data Index = Index {idxPos :: Pos, idxName :: String, idx :: Int, idxDepth :: Int}

instance Eq Index where
  (Index _ _ i1 d1) == (Index _ _ i2 d2) = i1 == i2 && d1 == d2
  
instance Ord Index where
  (Index _ _ i1 _) <= (Index _ _ i2 _) = i1 <= i2

instance Position Index where
  pos (Index p _ _ _) = p

instance Default Index where
  omission p = Index p "omission" (-1) (-1)

idxToVar :: Index -> Variable
idxToVar (Index p n _ _) = Variable p n

-- Nameless expressions and types
type NamelessExp = E.ExpOf Index
type NamelessType = T.TypeOf Index

-- The naming context is simply a list
type NamingCtx = [String]
-- Names from the prelude, in order. 
preludeNamingCtx :: NamingCtx
preludeNamingCtx = map intern $ Map.keys prelude

-- Operations on nameless representations
class Nameless t where
    removeNames :: NamingCtx -> Int -> t Variable -> FreestState (t Index)
    restoreNames :: t Index -> FreestState (t Variable)
    shift :: Int -> Int -> t Index -> t Index

-- Operations on nameless expressions
instance Nameless E.ExpOf where
    -- Basic values
    removeNames ctx _ (E.Int    p i) = return (E.Int    p i)
    removeNames ctx _ (E.Char   p c) = return (E.Char   p c)
    removeNames ctx _ (E.String p s) = return (E.String p s)
    removeNames ctx _ (E.Bool   p b) = return (E.Bool   p b)
    removeNames ctx _ (E.Unit   p  ) = return (E.Unit   p  )
    -- Term abstraction
    removeNames ctx d (E.Var pe x@(Variable pv name)) =
        case elemIndex name ctx of
            Nothing -> addError (VarOrConsNotInScope pv x) >> 
                       return (E.Var defaultPos (omission pv))
            Just i  -> return $ E.Var pe (Index pv name i d)
    removeNames ctx d (E.Abs p m b) = do
        binder' <- removeNames ctx (d + 1) (binder b)
        body'   <- removeNames (intern (var b) : ctx) (d + 1) (body b)
        return $ E.Abs p m b{binder=binder', body=body'}
    removeNames ctx d (E.App p e1 e2) =
        E.App p <$> removeNames ctx d e1
                <*> removeNames ctx d e2
    -- Pairs
    removeNames ctx d (E.Pair p e1 e2) =
        E.Pair p <$> removeNames ctx d e1
                 <*> removeNames ctx d e2
    removeNames ctx d (E.BinLet p v1 v2 e1 e2) =
        E.BinLet p v1 v2 <$> removeNames ctx d e1
                         <*> removeNames ctx' (d+1) e2
        where ctx' = intern v1 : intern v2 : ctx
    -- Datatypes
    removeNames ctx d (E.Case p e fm) = do
        E.Case p <$> removeNames ctx d e
                 <*> tMapM removeNamesBranch fm
        where removeNamesBranch (vs, ex) =
                (vs,) <$> removeNames (map intern vs ++ ctx) (d + length vs) ex
    -- Type abstraction
    removeNames ctx d (E.TypeAbs p b) = do
        body' <- removeNames (intern (var b) : ctx) (d + 1) (body b)
        return $ E.TypeAbs p b{body=body'}
    removeNames ctx d (E.TypeApp p e t) =
        E.TypeApp p <$> removeNames ctx d e
                    <*> removeNames ctx d t
    -- Conditional
    removeNames ctx d (E.Cond p e1 e2 e3) =
        E.Cond p <$> removeNames ctx d e1
                 <*> removeNames ctx d e2
                 <*> removeNames ctx d e3
    -- Let
    removeNames ctx d (E.UnLet p v e1 e2) =
        E.UnLet p v <$> removeNames ctx d e1
                    <*> removeNames ctx d e2
    -- New
    removeNames ctx d (E.New p t1 t2) =
        E.New p <$> removeNames ctx d t1 <*> removeNames ctx d t2

    -- Basic values
    restoreNames (E.Int    p i) = return $ E.Int    p i
    restoreNames (E.Char   p c) = return $ E.Char   p c
    restoreNames (E.String p s) = return $ E.String p s
    restoreNames (E.Bool   p b) = return $ E.Bool   p b
    restoreNames (E.Unit   p  ) = return $ E.Unit   p
    -- Term abstraction
    restoreNames (E.Var p i) = return $ E.Var p $ idxToVar i
    restoreNames (E.Abs p m b) = do
        binder' <- restoreNames (binder b)
        body' <- restoreNames (body b)
        return $  E.Abs p m b{binder=binder', body=body'}
    restoreNames (E.App p e1 e2) =
        E.App p <$> restoreNames e1 <*> restoreNames e2
    -- Pairs
    restoreNames (E.Pair p e1 e2) =
       E.Pair p <$> restoreNames e1 <*> restoreNames e2
    restoreNames (E.BinLet p v1 v2 e1 e2) =
        E.BinLet p v1 v2 <$> restoreNames e1 <*> restoreNames e2
    -- Datatypes
    restoreNames (E.Case p e fm) =
        E.Case p <$> restoreNames e
                 <*> tMapM restoreNamesBranch fm
        where restoreNamesBranch (vs, e') = (vs,) <$> restoreNames e'
    -- Type abstraction
    restoreNames (E.TypeAbs p b) = do
        body' <- restoreNames (body b)
        return $ E.TypeAbs p b{body=body'}
    restoreNames (E.TypeApp p e t) =
        E.TypeApp p <$> restoreNames e <*> restoreNames t
    -- Conditional
    restoreNames (E.Cond p e1 e2 e3) =
        E.Cond p <$> restoreNames e1
                 <*> restoreNames e2
                 <*> restoreNames e3
    restoreNames (E.UnLet p v e1 e2) =
        E.UnLet p v <$> restoreNames e1
                    <*> restoreNames e2
    restoreNames (E.New p t1 t2) =
        E.New p <$> restoreNames t1
                <*> restoreNames t2
    
    -- Basic Types
    shift d _ (E.Int    p i) = E.Int    p i
    shift d _ (E.Char   p c) = E.Char   p c
    shift d _ (E.String p s) = E.String p s
    shift d _ (E.Bool   p b) = E.Bool   p b
    shift d _ (E.Unit   p  ) = E.Unit   p
    -- Term abstraction
    shift d c (E.Var p i) | idx i < c = E.Var p i
                          | otherwise = E.Var p i{idx = idx i + d}
    shift d c (E.Abs p m b)           = E.Abs p m b{body = shift d (c + 1) (body b)}
    shift d c (E.App p e1 e2)         = E.App p (shift d c e1) (shift d c e2)
    -- Pairs
    shift d c (E.Pair p e1 e2)         = E.Pair p (shift d c e1) (shift d c e2)
    shift d c (E.BinLet p v1 v2 e1 e2) = E.BinLet p v1 v2 (shift d c e1) (shift d (c + 2) e2)
    -- Datatypes
    shift d c (E.Case p e fm) = E.Case p (shift d c e) fm' 
        where fm' = Map.map (\(vs, e') -> (vs, shift d (length vs) e')) fm'
    -- Type abstraction
    shift d c (E.TypeAbs p b)   = E.TypeAbs p b{body = shift d (c + 1) (body b)}
    shift d c (E.TypeApp p e t) = E.TypeApp p (shift d c e) (shift d c t)
    -- Conditional
    shift d c (E.Cond p e1 e2 e3) = E.Cond p (shift d c e1) (shift d c e2) (shift d c e3)
    -- Unary let
    shift d c (E.UnLet p v e1 e2) = E.UnLet p v (shift d c e1) (shift d (c + 1) e2)
    -- New
    shift d c (E.New p t1 t2) = E.New p (shift d c t1) (shift d c t2)
    
-- Operations on nameless types
instance Nameless T.TypeOf where
    -- REMOVE NAMES
    -- Functional types
    removeNames ctx _ (T.Int    p) = return $ T.Int    p
    removeNames ctx _ (T.Char   p) = return $ T.Char   p
    removeNames ctx _ (T.String p) = return $ T.String p
    removeNames ctx _ (T.Bool   p) = return $ T.Bool   p
    removeNames ctx _ (T.Unit   p) = return $ T.Unit   p
    removeNames ctx d (T.Arrow p m t1 t2) =
        T.Arrow p m <$> removeNames ctx d t1 <*> removeNames ctx d t2
    removeNames ctx d (T.Pair p t1 t2) =
        T.Pair p <$> removeNames ctx d t1 <*> removeNames ctx d t2
    removeNames ctx d (T.Almanac p s tm) =
        T.Almanac p s <$> tMapM (removeNames ctx d) tm
    -- Session types
    removeNames ctx _ (T.Skip p) = return $ T.Skip p
    removeNames ctx d (T.Semi p t1 t2) =
        T.Semi p <$> removeNames ctx d t1 <*> removeNames ctx d t2
    removeNames ctx d (T.Message pos pol t) =
        T.Message pos pol <$> removeNames ctx d t
    -- Polymorphic and recursive types
    removeNames ctx d (T.Forall p b) = do
        body' <- removeNames (intern (var b) : ctx) (d + 1) (body b)
        return $ T.Forall p b{body=body'}
    removeNames ctx d (T.Rec p b) = do
        body' <- removeNames (intern (var b) : ctx) (d + 1) (body b)
        return $ T.Rec p b{body=body'}
    removeNames ctx d (T.Var pe a@(Variable pv name)) =
        case elemIndex name ctx of
            Nothing -> addError (TypeVarNotInScope pe a) $> omission pe
            Just i -> return $ T.Var pe (Index pv name i d)
    -- Operators
    removeNames ctx d (T.Dualof p t) =
        T.Dualof p <$> removeNames ctx d t
    removeNames ctx d (T.CoVar pe a@(Variable pv name)) =
        case elemIndex name ctx of
            Nothing -> addError (TypeVarNotInScope pe a) $> omission pe
            Just i -> return $ T.CoVar pe (Index pv name i d)

    -- RESTORE NAMES
    -- Functional types
    restoreNames (T.Int    p) = return $ T.Int    p
    restoreNames (T.Char   p) = return $ T.Char   p
    restoreNames (T.String p) = return $ T.String p
    restoreNames (T.Bool   p) = return $ T.Bool   p
    restoreNames (T.Unit   p) = return $ T.Unit   p
    restoreNames (T.Arrow p m t1 t2) =
        T.Arrow p m <$> restoreNames t1 <*> restoreNames t2
    restoreNames (T.Pair p t1 t2) =
        T.Pair p <$> restoreNames t1 <*> restoreNames t2
    restoreNames (T.Almanac p s tm) =
        T.Almanac p s <$> tMapM restoreNames tm
    -- Session types
    restoreNames (T.Skip p) = return $ T.Skip p
    restoreNames (T.Semi p t1 t2) =
        T.Semi p <$> restoreNames t1 <*> restoreNames t2
    restoreNames (T.Message pos pol t) =
        T.Message pos pol <$> restoreNames t
    -- Polymorphic and recursive types
    restoreNames (T.Forall p b) = do
        body' <- restoreNames (body b)
        return $ T.Forall p b{body=body'}
    restoreNames (T.Rec p b) = do
        body' <- restoreNames (body b)
        return $ T.Rec p b{body=body'}
    restoreNames (T.Var p i) = return $ T.Var p $ idxToVar i
    -- Operators
    restoreNames (T.Dualof p t) = T.Dualof p <$> restoreNames t
    restoreNames (T.CoVar p i) = return $ T.Var p $ idxToVar i

    -- SHIFT
    -- Functional types
    shift d c (T.Int    p)        = T.Int    p
    shift d c (T.Char   p)        = T.Char   p
    shift d c (T.String p)        = T.String p
    shift d c (T.Bool   p)        = T.Bool   p
    shift d c (T.Unit   p)        = T.Unit   p
    shift d c (T.Arrow p m t1 t2) = T.Arrow p m (shift d c t1) (shift d c t2)
    shift d c (T.Pair p t1 t2)    = T.Pair p (shift d c t1) (shift d c t2)
    shift d c (T.Almanac p s tm)  = T.Almanac p s (Map.map (shift d c) tm)
    -- Session types
    shift d c (T.Skip p)            = T.Skip p
    shift d c (T.Semi p t1 t2)      = T.Semi p (shift d c t1) (shift d c t2)
    shift d c (T.Message pos pol t) = T.Message pos pol (shift d c t)
    -- Polymorphic and recursive types
    shift d c (T.Forall p b) = T.Forall p b{body = shift d (c + 1) (body b)}
    shift d c (T.Rec p b) = T.Rec p b{body = shift d (c + 1) (body b)}
    shift d c (T.Var p i) | idx i < c = T.Var p i
                          | otherwise = T.Var p i{idx = idx i + d}
    -- Operators
    shift d c (T.Dualof p t) = T.Dualof p (shift d c t)
    shift d c (T.CoVar p i) | idx i < c = T.CoVar p i
                            | otherwise = T.CoVar p i{idx = idx i + d}
    


{-
Unit tests
-- ...
   mapM_ (checkRemoveRestoreSpec . (!!0)) (chunksOf 2 t)

checkRemoveRestoreSpec :: String -> Spec
checkRemoveRestoreSpec e =
  it ("restorenames(Γ,removenames(Γ," ++ e ++ ")) = " ++ e) $ 
    isEqual (read e) `shouldBe` Left True

isEqual :: Exp -> TestExpectation
isEqual e =
  testValidExpectation (e == e') []
  where e' = evalState (removeNames preludeNamingCtx (length preludeNamingCtx) e >>= restoreNames) initialState
-}