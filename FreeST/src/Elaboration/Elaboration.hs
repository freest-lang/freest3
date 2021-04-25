{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections #-}
module Elaboration.Elaboration
  ( elaboration
  , Elaboration(..)
  )
where

import           Data.Functor
import           Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Elaboration.Duality           as Dual
import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Kind                   as K
import           Syntax.Program                 ( VarEnv )
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.Error                     ( internalError )
import           Util.FreestState
import           Util.PreludeLoader             ( userDefined )
import           Validation.Rename              ( isFreeIn )

elaboration :: FreestState ()
elaboration = do
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
--  debugM . ("Building recursive types" ++) <$> show =<< getTEnv

type Visited = Set.Set TypeVar

-- | Solve equations (TypeEnv)

solveEquations :: FreestState ()
solveEquations = buildRecursiveTypes >> solveAll >> cleanUnusedRecs
 where
  solveAll :: FreestState ()
  solveAll =
    getTEnv
      >>= tMapWithKeyM (\x (k, v) -> (k, ) <$> solveEq Set.empty x v)
      >>= setTEnv

  solveEq :: Visited -> TypeVar -> T.Type -> FreestState T.Type
  solveEq v f (T.Fun p m t1 t2) =
    T.Fun p m <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Pair p t1 t2) = T.Pair p <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Datatype p tm) = T.Datatype p <$> mapM (solveEq v f) tm
  solveEq v f (T.Semi p t1 t2) = T.Semi p <$> solveEq v f t1 <*> solveEq v f t2
  solveEq v f (T.Message p pol t) = T.Message p pol <$> solveEq v f t
  solveEq v f (T.Choice p pol tm) = T.Choice p pol <$> mapM (solveEq v f) tm
  solveEq v f t@(T.Var p x)
    | x `Set.member` v = pure t
    | f == x = pure t
    | otherwise = getFromTEnv x >>= \case
      Just tx -> solveEq (f `Set.insert` v) x (snd tx)
      Nothing ->
        addError p [Error "Type variable not in scope:", Error x] $> omission p
  solveEq v f (T.Forall p (K.Bind p1 x k t)) =
    T.Forall p . K.Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  solveEq v f (T.Rec p (K.Bind p1 x k t)) =
    T.Rec p . K.Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  solveEq v f (T.Dualof p t) = T.Dualof p <$> solveEq v f t
  solveEq _ _ p              = pure p


-- | Build recursive types

buildRecursiveTypes :: FreestState ()
buildRecursiveTypes = Map.mapWithKey buildRec <$> getTEnv >>= setTEnv
  where buildRec x (k, t) = (k, T.Rec (pos x) (K.Bind (pos x) x k t))

-- | Clean rec types where the variable does not occur free

cleanUnusedRecs :: FreestState ()
cleanUnusedRecs = Map.mapWithKey clean <$> getTEnv >>= setTEnv
 where
  clean x u@(k, T.Rec _ (K.Bind _ _ _ t)) | x `isFreeIn` t = u
                                          | otherwise      = (k, t)
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

-- | Elaboration: Substitutions over Type, Exp, TypeMap, FieldMap, and Binds

class Elaboration t where
  elaborate :: t -> FreestState t

instance Elaboration T.Type where
  elaborate (  T.Message p pol t) = T.Message p pol <$> elaborate t
  elaborate (  T.Fun p m t1 t2  ) = T.Fun p m <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Pair p t1 t2   ) = T.Pair p <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Datatype p m   ) = T.Datatype p <$> elaborate m
  elaborate (  T.Semi   p t1  t2) = T.Semi p <$> elaborate t1 <*> elaborate t2
  elaborate (  T.Choice p pol m ) = T.Choice p pol <$> elaborate m
  elaborate (  T.Forall p kb    ) = T.Forall p <$> elaborate kb
  elaborate (  T.Rec    p kb    ) = T.Rec p <$> elaborate kb
  elaborate n@(T.Var    p tname ) = getFromTEnv tname >>= \case
    Just t  -> addTypeName p n >> pure (changePos p (snd t))
    Nothing -> pure n
  elaborate (T.Dualof p t) = T.Dualof p <$> elaborate t
  elaborate t              = pure t

-- Apply elaborateType over TypeMaps
instance Elaboration T.TypeMap where
  elaborate = mapM elaborate

instance Elaboration a => Elaboration (K.Bind a) where
  elaborate (K.Bind p x k a) = K.Bind p x k <$> elaborate a

-- instance Elaboration (K.Bind Exp) where
--   elaborate (K.Bind p x k e) = K.Bind p x k <$> elaborate e

instance Elaboration Bind where
  elaborate (Bind p m x t e) = Bind p m x <$> elaborate t <*> elaborate e

-- Substitute expressions

instance Elaboration Exp where
  elaborate (Abs p b     ) = Abs p <$> elaborate b
  elaborate (App  p e1 e2) = App p <$> elaborate e1 <*> elaborate e2
  elaborate (Pair p e1 e2) = Pair p <$> elaborate e1 <*> elaborate e2
  elaborate (BinLet p x y e1 e2) =
    BinLet p x y <$> elaborate e1 <*> elaborate e2
  elaborate (Case p e m) = Case p <$> elaborate e <*> elaborate m
  elaborate (Cond p e1 e2 e3) =
    Cond p <$> elaborate e1 <*> elaborate e2 <*> elaborate e3
  elaborate (TypeApp p e t  ) = TypeApp p <$> elaborate e <*> elaborate t
  elaborate (TypeAbs p b    ) = TypeAbs p <$> elaborate b
  elaborate (UnLet p x e1 e2) = UnLet p x <$> elaborate e1 <*> elaborate e2
  elaborate (New p t u      ) = New p <$> elaborate t <*> elaborate u
  elaborate e                 = return e

instance Elaboration FieldMap where
  elaborate = mapM (\(ps, e) -> (ps, ) <$> elaborate e)


-- | Build a program from the parse env

buildProg :: FreestState ()
buildProg = getPEnv
  >>= tMapWithKeyM_ (\pv (ps, e) -> addToProg pv =<< buildFunBody pv ps e)
 where
  buildFunBody :: ProgVar -> [ProgVar] -> Exp -> FreestState Exp
  buildFunBody f as e = getFromVEnv f >>= \case
    Just s  -> return $ buildExp e as s
    Nothing -> do
      addError
        (pos f)
        [ Error "The binding for function"
        , Error f
        , Error "lacks an accompanying type signature"
        ]
      return e

  buildExp :: Exp -> [ProgVar] -> T.Type -> Exp
  buildExp e [] _ = e
  buildExp e (b : bs) (T.Fun _ m t1 t2) =
    Abs (pos b) (Bind (pos b) m b t1 (buildExp e bs t2))
  buildExp _ _ t@(T.Dualof _ _) =
    internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp e bs (T.Forall p (K.Bind p1 x k t)) =
    TypeAbs p (K.Bind p1 x k (buildExp e bs t))
  buildExp e (b : bs) t =
    Abs (pos b) (Bind (pos b) Un b (omission (pos b)) (buildExp e bs t))


-- | Changing positions

-- Change position of a given type with a given position
changePos :: Pos -> T.Type -> T.Type
changePos p (T.Int  _         ) = T.Int p
changePos p (T.Char _         ) = T.Char p
changePos p (T.Bool _         ) = T.Bool p
changePos p (T.Unit _         ) = T.Unit p
changePos p (T.Fun _ pol t u  ) = T.Fun p pol (changePos p t) (changePos p u)
changePos p (T.Pair    _ t   u) = T.Pair p t u
-- Datatype
-- Skip
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Choice  _ pol m) = T.Choice p pol m
changePos p (T.Rec    _ xs    ) = T.Rec p xs
changePos p (T.Forall _ xs    ) = T.Forall p xs
-- TypeVar
changePos _ t                   = t
