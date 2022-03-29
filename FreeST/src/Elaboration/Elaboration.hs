{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns #-}
module Elaboration.Elaboration
  ( elaboration
  , Elaboration(..)
  )
where

import           Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Elaboration.ResolveDuality    as Dual
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program                 ( VarEnv, TypeEnv )
import qualified Syntax.Type                   as T
import           Util.Error
import           Util.FreestState
import           Util.PreludeLoader             ( userDefined )
import           Validation.Rename              ( isFreeIn )
import           Validation.Substitution              ( subs )
import           Validation.Rename

elaboration :: FreestState ()
elaboration = do
  -- | Builds recursive types
  -- Performs the proper substitutions (for type applications)
  buildRecTypes
  -- | Solve the equations' system.
  solveEquations
  -- | From this point, there are no type names on the RHS
  --   of the type declarations and datatypes (type env)
  -- | Substitute all type names on the function signatures
  substitute =<< getVEnv
  -- | same for parse env (which contains the functions' bodies)
  substitute =<< getPEnv
  -- | From this point, there are no type names on the function signatures
  --   and on the function bodies. 
  -- | Then, resolve all the dualof occurrences on:
  -- | Type Env (i.e. type A = dualof !Int)
  (Dual.resolve =<< getTEnv) >>= setTEnv
  -- | Var Env (i.e. f : dualof !Int -> Skip)
  (Dual.resolve =<< getVEnv) >>= setVEnv
  -- | Parse Env (i.e. f c = send 5 c)
  (Dual.resolve =<< getPEnv) >>= setPEnv
  -- | From this point there are no more occurrences of the dualof operator
  -- | Build the expression environment: substitute all
  --   type operators on ExpEnv;
  --   From f x = E and f : T -> U
  --   build a lambda expression: f = \x : T -> E
  buildProg
  -- debugM . ("Building recursive types" ++) <$> show =<< getTEnv
  -- debugM . ("VEnv" ++) <$> show . userDefined =<< getVEnv
 

type Visited = Set.Set Variable


-- | Builds recursive types
-- 
-- Assumes that each type abbreviation and datatype:
-- ex: type LC a:MU = +{ Cons: !a;(LC a), Nil: Skip }
-- comes (from the parser) as
-- type LC a:MU = \a:MU -> rec LC . +{ Cons: !a;(LC a), Nil: Skip }
--
-- Then, it traverses the type env and transforms each type abbrv/datatype into:
-- type LC a:MU = \a:MU -> rec LC . +{ Cons: !a;LC, Nil: Skip }
-- issues an error if the application is ill-formed.

buildRecTypes :: FreestState ()
buildRecTypes = getTEnv >>= tMapWithKeyM simplifyRec >>= setTEnv 

simplifyRec :: Variable -> (K.Kind, T.Type) -> FreestState (K.Kind, T.Type)
simplifyRec name (k, u) = (k,) <$> subsApp u
  where
    -- acc :: T.Type -> initially is the name of the type abbreviation
    buildApp :: T.Type -> T.Type -> T.Type
    buildApp acc (T.Abs _ (Bind _ x _ t)) = buildApp (T.App (pos acc) acc (T.Var (pos x) x)) t
    buildApp acc _ = acc

    subsApp :: T.Type -> FreestState T.Type
    subsApp (T.Arrow p m t1 t2) = T.Arrow p m <$> subsApp t1 <*> subsApp t2
    subsApp (T.Pair p t1 t2) = T.Pair p <$> subsApp t1 <*> subsApp t2
    subsApp (T.Variant p tm) = T.Variant p <$> mapM subsApp tm
    subsApp (T.Semi p t1 t2) = T.Semi p <$> subsApp t1 <*> subsApp t2
    subsApp (T.Message p pol t) = T.Message p pol <$> subsApp t
    subsApp (T.Choice p pol tm) = T.Choice p pol  <$> mapM subsApp tm
    subsApp (T.Forall p (Bind p1 x k t)) = T.Forall p . Bind p1 x k <$> subsApp t
    subsApp (T.Rec p (Bind p1 x k t)) = T.Rec p . Bind p1 x k <$> subsApp t
    subsApp (T.Dualof p t) = T.Dualof p <$> subsApp t
    subsApp t@T.App{} =
      let app   = buildApp (T.Var (pos name) name) u
          equal = compareApp app t in
        if equal
        then pure $ T.Var (pos name) name
        else addError (WrongTypeApp (pos t) app t) $> t
    subsApp (T.Abs p (Bind p1 x k t)) = T.Abs p . Bind p1 x k <$> subsApp t
    subsApp p = pure p
 
    compareApp :: T.Type -> T.Type -> Bool
    compareApp (T.App _ t1 u1) (T.App _ t2 u2) = compareApp t1 t2 && compareApp u1 u2
    compareApp (T.Var _ x)     (T.Var _ y)     = x == y
    compareApp _ _                             = False 
 
-- | Solve equations (TypeEnv)

solveEquations :: FreestState ()
solveEquations = solveAll >> cleanUnusedRecs
 where
  solveAll :: FreestState ()
  solveAll =
    getTEnv
      >>= tMapWithKeyM (\x (k, v) -> (k, ) <$> solveEq Set.empty x v)
      >>= setTEnv

  solveEq :: Visited -> Variable -> T.Type -> FreestState T.Type
  solveEq v f (T.Arrow p m t1 t2) =
    T.Arrow p m <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Pair p t1 t2) = T.Pair p <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Variant p tm) = T.Variant p <$> mapM (solveEq v f) tm
  solveEq v f (T.Semi p t1 t2) = T.Semi p <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Message p pol t) = T.Message p pol <$> solveEq v f t
  solveEq v f (T.Choice p pol tm) = T.Choice p pol <$> mapM (solveEq v f) tm
  solveEq v f t@(T.Var p x)
    | x `Set.member` v = pure t
    | f == x = pure t
    | otherwise = getFromTEnv x >>= \case
      Just tx -> solveEq (f `Set.insert` v) x (snd tx)
      Nothing -> addError (TypeVarOutOfScope p x) $> omission p
  solveEq v f (T.Forall p (Bind p1 x k t)) =
    T.Forall p . Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  solveEq v f (T.Rec p (Bind p1 x k t)) =
    T.Rec p . Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  solveEq v f (T.Dualof p t) = T.Dualof p <$> solveEq v f t
  solveEq v f (T.App p t u) = T.App p <$> solveEq v f t <*> solveEq v f u
  solveEq v f (T.Abs p (Bind p1 x k t)) =
    T.Abs p . Bind p1 x k <$> solveEq (x `Set.insert` v) f t
--    T.Abs p . Bind p1 x k <$> solveEq v f t
  solveEq _ _ p              = pure p

cleanUnusedRecs :: FreestState ()
cleanUnusedRecs = Map.mapWithKey (\x (k,t) -> (k, clean x t)) <$> getTEnv >>= setTEnv
  where
    clean x (T.Abs p (Bind p1 x1 k1 t)) = T.Abs p $ Bind p1 x1 k1 $ clean x t
    clean x (T.Rec p (Bind p' y k t))
      | x `isFreeIn` t = T.Rec p $ Bind p' y k $ clean x t
      | otherwise      = t    
    clean _ kt = kt

-- | Substitutions over environment (VarEnv + ParseEnv)

class Substitution t where
  substitute :: t -> FreestState ()

-- | Substitute on function signatures (VarEnv)

instance Substitution VarEnv where
  substitute =
    tMapWithKeyM_ (\pv t -> addToVEnv pv =<< elaborate t) . userDefined

-- | Substitute Types on the expressions (ParseEnv)

instance Substitution ParseEnv where
  substitute = tMapWithKeyM_ (\x (ps, e) -> addToPEnv x ps =<< elaborate e)


instance Substitution TypeEnv where
  substitute = tMapWithKeyM_ (\x (k, t) -> addToTEnv x k =<< elaborate t)


-- | Elaboration: Substitutions over Type, Exp, TypeMap, FieldMap, and Binds

class Elaboration t where
  elaborate :: t -> FreestState t

instance Elaboration T.Type where
  elaborate (  T.Message p pol t) = T.Message p pol <$> elaborate t
  elaborate (  T.Arrow p m t1 t2) = T.Arrow p m <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Pair p t1 t2   ) = T.Pair p <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Variant p m    ) = T.Variant p <$> elaborate m
  elaborate (  T.Semi   p t1  t2) = T.Semi p <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Choice p pol m ) = T.Choice p pol <$> elaborate m
  elaborate (  T.Forall p kb    ) = T.Forall p <$> elaborate kb
  elaborate (  T.Rec    p kb    ) = T.Rec p <$> elaborate kb
  elaborate n@(T.Var    p tname ) = getFromTEnv tname >>= \case
    Just t  -> addTypeName p n >> pure (changePos p (snd t))
    Nothing -> pure n
  elaborate (  T.Dualof p t     ) = T.Dualof p <$> elaborate t
  elaborate (  T.App    p t    u) = T.App p <$> elaborate t <*> elaborate u
  elaborate (  T.Abs    p b    ) =  T.Abs p <$> elaborate b
  -- elaborate (  T.Abs    p (Bind p' x k a)    ) =
  --   T.Abs p . Bind p' x k <$> (elaborate =<< rename Map.empty a)
  elaborate t                     = pure t

-- Apply elaborateType over TypeMaps
instance Elaboration T.TypeMap where
  elaborate = mapM elaborate

instance Elaboration a => Elaboration (Bind K.Kind a) where
  elaborate (Bind p x k a) = Bind p x k <$> elaborate a

-- instance Elaboration (Bind K.Kind Exp) where
--   elaborate (Bind p x k e) = Bind p x k <$> elaborate e

instance Elaboration (Bind T.Type E.Exp) where
  elaborate (Bind p x t e) = Bind p x <$> elaborate t <*> elaborate e

-- Substitute expressions

instance Elaboration E.Exp where
  elaborate (E.Abs p m b   ) = E.Abs p m <$> elaborate b
  elaborate (E.App  p e1 e2) = E.App p <$> elaborate e1 <*> elaborate e2
  elaborate (E.Pair p e1 e2) = E.Pair p <$> elaborate e1 <*> elaborate e2
  elaborate (E.BinLet p x y e1 e2) =
    E.BinLet p x y <$> elaborate e1 <*> elaborate e2
  elaborate (E.Case p e m) = E.Case p <$> elaborate e <*> elaborate m
  elaborate (E.Cond p e1 e2 e3) =
    E.Cond p <$> elaborate e1 <*> elaborate e2 <*> elaborate e3
  elaborate (E.TypeApp p e t  ) = E.TypeApp p <$> elaborate e <*> elaborate t
  elaborate (E.TypeAbs p b    ) = E.TypeAbs p <$> elaborate b
  elaborate (E.UnLet p x e1 e2) = E.UnLet p x <$> elaborate e1 <*> elaborate e2
  elaborate (E.New p t u      ) = E.New p <$> elaborate t <*> elaborate u
  elaborate e                 = return e

instance Elaboration E.FieldMap where
  elaborate = mapM (\(ps, e) -> (ps, ) <$> elaborate e)


-- | Build a program from the parse env

buildProg :: FreestState ()
buildProg = getPEnv
  >>= tMapWithKeyM_ (\pv (ps, e) -> addToProg pv =<< buildFunBody pv ps e)
 where
  buildFunBody :: Variable -> [Variable] -> E.Exp -> FreestState E.Exp
  buildFunBody f as e = getFromVEnv f >>= \case
    Just s  -> return $ buildExp e as s
    Nothing -> addError (FuctionLacksSignature (pos f) f) $> e
      
  buildExp :: E.Exp -> [Variable] -> T.Type -> E.Exp
  buildExp e [] _ = e
  buildExp e (b : bs) (T.Arrow _ m t1 t2) =
    E.Abs (pos b) m (Bind (pos b) b t1 (buildExp e bs t2))
  buildExp _ _ t@(T.Dualof _ _) =
    internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    E.TypeAbs p (Bind p1 x k (buildExp e bs t))
  buildExp e (b : bs) t =
    E.Abs (pos b) Un (Bind (pos b) b (omission (pos b)) (buildExp e bs t))

-- | Changing positions

-- Change position of a given type with a given position
changePos :: Pos -> T.Type -> T.Type
changePos p (T.Int  _         ) = T.Int p
changePos p (T.Char _         ) = T.Char p
changePos p (T.Bool _         ) = T.Bool p
changePos p (T.Unit _         ) = T.Unit p
changePos p (T.Arrow _ pol t u) = T.Arrow p pol (changePos p t) (changePos p u)
changePos p (T.Pair    _ t   u) = T.Pair p t u
-- Datatype
-- Skip
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Choice  _ pol m) = T.Choice p pol m
changePos p (T.Rec    _ xs    ) = T.Rec p xs
changePos p (T.Forall _ xs    ) = T.Forall p xs
changePos p (T.Abs    _ b     ) = T.Abs p b
changePos p (T.App _ t u      ) = T.App p t u
-- Variable
changePos _ t                   = t
