{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Elaboration.Elaboration
  ( elaboration
  , Elaboration(..)
  )
where

import           Data.Functor
import           Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Kind                   as K
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.Error                    ( internalError )
import           Util.FreestState
import           Util.PreludeLoader            ( userDefined )
import           Validation.Kinding             ( synthetise )
import           Validation.Rename              ( isFreeIn )

elaboration :: FreestState ()
elaboration = do
  -- | Build data and type declarations as recursive types
  buildRecursiveTypes
--  debugM . ("Building recursive types" ++) <$> show =<< getTEnv
  -- | Solve the equations
  solveEquations
  -- | Note: From this point on there are no type names on type env
  -- | Resolve all occurrences of the dualof type operator
  resolveDualof
  -- | Note: From this point on there are no occurrences of type dualof
  -- | Remove recursive types (rec x . T) where the recursion   
  -- | variable x does not occur free in T
  cleanUnusedRecs
  -- | Check if the substituted types are contractive
  mapM_ (synthetise Map.empty . snd) =<< getTEnv -- TODO: why do we need this here?
  -- | Substitute all type operators (type names?) on VarEnv
  substituteVEnv
  -- | Note: From this point on function types (all types?) contain no
  -- | type names neither dualofs TODO: Don't understand
  -- | Build the expression environment: substitute all
  -- | type operators on ExpEnv;
  -- | From f x = E and f : T -> U
  -- | build a lambda expression: f = \x : T -> E
  substitutePEnv


-- | Build recursive types

buildRecursiveTypes :: FreestState ()
buildRecursiveTypes = Map.mapWithKey buildRec <$> getTEnv >>= setTEnv
  where buildRec x (k, t) = (k, T.Rec (pos x) (K.Bind (pos x) x k t))

type Visited = Set.Set TypeVar

-- | Solve equations (TypeEnv)

solveEquations :: FreestState ()
solveEquations =
  getTEnv
    >>= tMapWithKeyM (\x (k, v) -> (,) k <$> solveEq Set.empty x v)
    >>= setTEnv
 where
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
  solveEq v f (T.Forall p (K.Bind p1 x k t)) = -- TODO: Should we insert on visited?
    T.Forall p . K.Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  solveEq v f (T.Rec p (K.Bind p1 x k t)) =
    T.Rec p . K.Bind p1 x k <$> solveEq (x `Set.insert` v) f t
  -- solveEq v f (T.Abs p b t) =  -- λ a:k => T  
    --   fmap (T.Abs p b) (solveEq v f t)
  -- solveEq v f (T.App p t1 t2) =
    --   liftM2 (T.App p) (solveEq v f t1) (solveEq v f t2)
  solveEq v f (T.Dualof p t) =
    T.Dualof p <$> solveEq v f t
  solveEq _ _ p            = pure p

-- | Resolving the dualof operator

resolveDualof :: FreestState ()
resolveDualof =
  getTEnv >>= tMapM (\(k, t) -> (,) k <$> solveType Set.empty t) >>= setTEnv

solveType :: Visited -> T.Type -> FreestState T.Type
-- Functional Types
solveType v (T.Fun p pol t u) =
  T.Fun p pol <$> solveType v t <*> solveType v u
solveType v (T.Pair p t u) =
  T.Pair p <$> solveType v t <*> solveType v u
solveType v (T.Datatype p m) = T.Datatype p <$> tMapM (solveType v) m
-- Session Types
solveType v (T.Semi p t u) = T.Semi p <$> solveType v t <*> solveType v u
solveType v (T.Message p pol t ) = T.Message p pol <$> solveType v t
solveType v (T.Choice p pol m) = T.Choice p pol <$> tMapM (solveType v) m
-- Polymorphism and recursive types
solveType v (T.Forall p (K.Bind p' a k t)) =
  T.Forall p . K.Bind p' a k <$> solveType v t
solveType v (T.Rec p b) = T.Rec p <$> solveBindT v b
solveType _ t@T.Var{} = pure t
-- Dualof
solveType v d@(T.Dualof p t) = addTypeName p d >> solveDual v t
-- Int, Char, Bool, Unit, Skip
solveType _ p = pure p

solveDual :: Visited -> T.Type -> FreestState T.Type
-- Session Types
solveDual _ t@T.Skip{} = pure t
solveDual v (T.Semi p t u) = T.Semi p <$> solveDual v t <*> solveDual v u
solveDual v (T.Message p pol t) = T.Message p (dualPol pol) <$> solveType v t
solveDual v (T.Choice p pol m) =
  T.Choice p (dualPol pol) <$> tMapM (solveDual v) m
-- Recursive types
solveDual v (T.Rec p b) = T.Rec p <$> solveBindD v b
solveDual v t@(T.Var p a)
  | a `Set.member` v = pure t -- A recursion variable
  | otherwise = 
      addError p
      [Error "Cannot compute the dual of a non-recursion variable:"
      , Error t] $> t
  -- | otherwise = getFromTEnv a >>= \case
  --   Just (_, u) -> solveDual v u
  --   Nothing -> -- A polymorphic variable
  --     addError p
  --     [Error "Cannot compute the dual of a non-recursion variable:"
  --     , Error a] $> t
-- Dualof
solveDual v d@(T.Dualof p t) = addTypeName p d >> solveType v t
-- Non session-types
solveDual _ t =
      addError (pos t)
      [Error "Dualof applied to a non session type: "
      , Error t] $> t
  
solveBindT :: Visited -> K.Bind T.Type -> FreestState (K.Bind T.Type)
solveBindT v (K.Bind p a k t) =
  K.Bind p a k <$> solveType (Set.insert a v) t

solveBindD :: Visited -> K.Bind T.Type -> FreestState (K.Bind T.Type)
solveBindD v (K.Bind p a k t) =
  K.Bind p a k <$> solveDual (Set.insert a v) t

dualPol :: T.Polarity -> T.Polarity
dualPol T.In  = T.Out
dualPol T.Out = T.In

-- | Clean rec types where the variable does not occur free

cleanUnusedRecs :: FreestState ()
cleanUnusedRecs = Map.mapWithKey clean <$> getTEnv >>= setTEnv
 where
  clean x u@(k, T.Rec _ (K.Bind _ _ _ t)) | x `isFreeIn` t = u
                                          | otherwise      = (k, t)
  clean _ kt = kt

-- | Substitute on function signatures (VarEnv)

substituteVEnv :: FreestState ()
substituteVEnv =
  tMapWithKeyM_ (\pv t -> addToVEnv pv =<< elaborate t)
    .   userDefined
    =<< getVEnv

-- | Substitute Types on the expressions (ExpEnv)

substitutePEnv :: FreestState ()
substitutePEnv = getPEnv >>= tMapWithKeyM_
  (\pv (ps, e) -> addToProg pv =<< buildFunBody pv ps =<< elaborate e)


buildFunBody :: ProgVar -> [ProgVar] -> Exp -> FreestState Exp
buildFunBody f as e = getFromVEnv f >>= \case
  Just s  -> return $ buildExp as s
  Nothing -> do
    addError
      (pos f)
      [ Error "The binding for function"
      , Error f
      , Error "lacks an accompanying type signature"
      ]
    return e
 where
  buildExp :: [ProgVar] -> T.Type -> Exp
  buildExp [] _ = e
  buildExp (b : bs) (T.Fun _ m t1 t2) =
    Abs (pos b) (Bind (pos b) m b t1 (buildExp bs t2))
  buildExp _ t@(T.Dualof _ _) =
    internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp bs (T.Forall p (K.Bind p1 x k t)) =
    TypeAbs p (K.Bind p1 x k (buildExp bs t))
  buildExp (b : bs) t =
    Abs (pos b) (Bind (pos b) Un b (omission (pos b)) (buildExp bs t))


-- | Elaboration

class Elaboration t where
  elaborate :: t -> FreestState t


instance Elaboration T.Type where
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
  elaborate n@(T.Dualof p t) =
    addTypeName p n >>
    changePos p <$> (solveDual Set.empty =<< elaborate t)
  elaborate t = pure t

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
  elaborate e@Select{}        = pure e
  elaborate (Match p e m)     = Match p <$> elaborate e <*> elaborate m
  elaborate e                 = return e


instance Elaboration FieldMap where
  elaborate = mapM (\(ps, e) -> (,) ps <$> elaborate e)

-- | Changing positions

-- Change position of a given type with a given position
changePos :: Pos -> T.Type -> T.Type
changePos p (T.Int  _         ) = T.Int p
changePos p (T.Char _         ) = T.Char p
changePos p (T.Bool _         ) = T.Bool p
changePos p (T.Unit _         ) = T.Unit p
changePos p (T.Fun _ pol t u  ) = T.Fun p pol t u
changePos p (T.Pair    _ t   u) = T.Pair p t u
-- Datatype
-- Skip
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Choice  _ pol m) = T.Choice p pol m
changePos p (T.Rec    _ xs    ) = T.Rec p xs -- (changePos p t)
changePos p (T.Forall _ xs    ) = T.Forall p xs -- (changePos p t)
-- TypeVar
changePos _ t                   = t
