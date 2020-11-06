{-# LANGUAGE LambdaCase #-}
module Validation.BuildTypes where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Base
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Validation.Duality                 ( dual )
import           Validation.Terminated
import           Validation.Kinding             ( synthetiseTS )
import           Utils.FreestState
import           Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Debug.Trace
import           Control.Monad                  ( liftM, liftM2, liftM3 )
import           Utils.PreludeLoader            ( userDefined ) -- debug


solveTypeDecls :: FreestState ()
solveTypeDecls = do
  tenv <- getTEnv
  let tenv' = typeDecls tenv
--  traceM $ "\n1. INITIAL ENV: " ++ show tenv' ++ "\n"
  -- Solve the system of equations
  eqs  <- solveEqs tenv'
--  traceM $ "\n2. TYPENAMES: " ++ show eqs ++ "\n"
  -- Replace all occurrences of DualOf t
  eqs' <- solveDualOfs eqs
--  traceM $ "\n3. DUALOFS: " ++ show eqs' ++ "\n"
  -- Check if the substituted types are contractive  
  mapM_ (synthetiseTS Map.empty . snd) eqs'
--  mapM_ (checkContractive Map.empty . snd) eqs'
  -- Substitute all type operators on VarEnv
  substituteVEnv eqs'
  -- Substitute all type operators on ExpEnv
  substituteEEnv eqs'


-- PHASE 1: SOLVE THE SYSTEM OF EQUATIONS

solveEqs :: TypeEnv -> FreestState TypeEnv
solveEqs tenv = Map.foldlWithKey solveEq (return tenv) tenv
 where
  solveEq
    :: FreestState TypeEnv
    -> TypeVar
    -> (Kind, TypeScheme)
    -> FreestState TypeEnv
  solveEq acc x (k, s) = do
    let bt = buildRecursiveType x (k, s)
    fmap (Map.insert x (k, fromType bt)) acc >>= substituteEnv x bt

-- substitute every occurence of variable x in all the other entries of the map
substituteEnv :: TypeVar -> Type -> TypeEnv -> FreestState TypeEnv -- TODO: refactor
substituteEnv x t = tMapWithKeyM subsEnv
 where
  subsEnv :: TypeVar -> (Kind, TypeScheme) -> FreestState (Kind, TypeScheme)
  subsEnv v ks@(k, TypeScheme p b s)
    | x == v = pure ks
    | -- ignore the node itself
      otherwise = do
      s' <- subsType Map.empty (Just (x, t)) s
      let bt = buildRecursiveType v (k, TypeScheme p b s')
      return (k, TypeScheme p b bt)

-- GETTING ONLY TYPE DECLS FROM TENV (IGNORING DATATYPES)

typeDecls :: TypeEnv -> TypeEnv
typeDecls = Map.filter (not . isDataType . snd )

isDataType :: TypeScheme -> Bool
isDataType (TypeScheme _ _ (Datatype _ _)) = True
isDataType _                               = False

-- BUILDING RECURSIVE TYPES IF NEEDED

buildRecursiveType :: TypeVar -> (Kind, TypeScheme) -> Type
buildRecursiveType v (k, TypeScheme _ _ t)
  | isRecursiveTypeDecl v t = Rec (position v)
                                  (KindBind (position v) v k)
                                  (toTypeVar v t)
  | otherwise = t

isRecursiveTypeDecl :: TypeVar -> Type -> Bool
isRecursiveTypeDecl v (Semi _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (Choice _ _ m) =
  Map.foldlWithKey (\b _ t -> b || isRecursiveTypeDecl v t) False m
isRecursiveTypeDecl v (Rec _ (KindBind _ x _) t)
  | x == v    = False
  | -- it is already a recursive type
    otherwise = isRecursiveTypeDecl v t
isRecursiveTypeDecl v (TypeName _ x) = x == v
isRecursiveTypeDecl v (TypeVar  _ x) = x == v
isRecursiveTypeDecl v (Fun _ _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (PairType _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (Dualof _ t) = isRecursiveTypeDecl v t
isRecursiveTypeDecl _ _            = False


-- Convert typenames to typeVars; when a type declaration is converted in a rec type
-- Added the additional type var because datatypes are also typenames
toTypeVar :: TypeVar -> Type -> Type
toTypeVar x (Choice p pol m) = Choice p pol (Map.map (toTypeVar x) m)
toTypeVar x (TypeName p tname) | --
                                 x == tname = TypeVar p tname
                               | otherwise  = TypeName p tname
toTypeVar x (Semi p t1 t2) = Semi p (toTypeVar x t1) (toTypeVar x t2)
toTypeVar _ (Rec p xs@(KindBind _ x _) t) = Rec p xs (toTypeVar x t)
-- functional types
toTypeVar x (Fun p m t u) = Fun p m (toTypeVar x t) (toTypeVar x u)
toTypeVar x (PairType p t u) = PairType p (toTypeVar x t) (toTypeVar x u)
-- Datatype
toTypeVar _ t = t

-- PHASE 2 - SOLVING DUALOF TYPE OPERATORS

solveDualOfs :: TypeEnv -> FreestState TypeEnv
solveDualOfs tenv = tMapM
  (\(k, TypeScheme p xs t) ->
    solveDualOf tenv t >>= \t' -> pure (k, TypeScheme p xs t')
  )
  tenv

solveDualOf :: TypeEnv -> Type -> FreestState Type
solveDualOf tenv (Choice p pol m) =
  fmap (Choice p pol) (tMapM (solveDualOf tenv) m)
solveDualOf tenv (Semi p t u) =
  liftM2 (Semi p) (solveDualOf tenv t) (solveDualOf tenv u)
solveDualOf tenv (Rec p xs t) = fmap (Rec p xs) (solveDualOf tenv t)
solveDualOf tenv (Fun p pol t u) =
  liftM2 (Fun p pol) (solveDualOf tenv t) (solveDualOf tenv u)
solveDualOf tenv n@(TypeName _ tname) = case tenv Map.!? tname of
  Just (_, TypeScheme _ _ t) -> pure (toTypeVar tname t)
  Nothing                    -> maybeScopeErr n
solveDualOf tenv d@(Dualof p t) = do
  addTypeName p d
  u <- solveDualOf tenv t
  dual u
--  fmap dual (solveDualOf tenv t)
solveDualOf _ p = return p


-- Until we have datatypes as typenames; we have to filter non-datatypes
-- In other words we have to check if it is on the original map
-- TODO: When datatypes become recursive types as well; one should keep
-- only the Nothing (error) case ?
maybeScopeErr :: Type -> FreestState Type
maybeScopeErr (TypeName p tname) = getFromTEnv tname >>= \case
  Just (_, TypeScheme _ _ t) -> pure t
  Nothing -> addError p [Error "Type name not in scope:", Error tname]
    >> pure (Basic p UnitType)

-- Change position of a given type with a given position
changePos :: Pos -> Type -> Type
changePos p (Basic _ t       ) = Basic p t
changePos p (Fun _ pol t u   ) = Fun p pol t u
changePos p (PairType _ t   u) = PairType p t u
-- Datatype
-- Skip
changePos p (Semi     _ t   u) = Semi p t u
changePos p (Message  _ pol b) = Message p pol b
changePos p (Choice   _ pol m) = Choice p pol m
changePos p (Rec      _ xs  t) = Rec p xs t -- (changePos p t)
-- TypeVar
changePos _ t                  = t


-- PHASE 3: SUBSTITUTE ON FUNCTION SIGNATURES (VARENV)

substituteVEnv :: TypeEnv -> FreestState ()
substituteVEnv tenv = getVEnv >>= tMapWithKeyM_ subsElem
 where
  subsElem :: ProgVar -> TypeScheme -> FreestState ()
  subsElem pv (TypeScheme p b s) = do
    s' <- subsType tenv Nothing s
    addToVEnv pv (TypeScheme p b s')

-- PHASE 4: SUBSTITUTE TYPES ON THE EXPRESSIONS (EXPENV)

substituteEEnv :: TypeEnv -> FreestState ()
substituteEEnv tenv = getEEnv >>= \eenv -> tMapWithKeyM_ subsUpdateExp eenv
 where
  subsUpdateExp :: ProgVar -> Expression -> FreestState ()
  subsUpdateExp pv e = subsExp tenv e >>= \e1 -> addToEEnv pv e1


-- SUBSTITUTIONS

-- Substitute a type

subsType :: TypeEnv -> Maybe (TypeVar, Type) -> Type -> FreestState Type
subsType tenv b (Fun p m t1 t2) =
  liftM2 (Fun p m) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b (PairType p t1 t2) =
  liftM2 (PairType p) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b (Datatype p m) = fmap (Datatype p) (subsMap tenv b m)
subsType tenv b (Semi p t1 t2) =
  liftM2 (Semi p) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b (Choice p pol m ) = fmap (Choice p pol) (subsMap tenv b m)
subsType tenv b (Rec    p tvb t1) = fmap (Rec p tvb) (subsType tenv b t1)
-- In the first phase, we only substitute if the typename is the one that
-- we are looking for (x)
subsType _ (Just (x, t)) n@(TypeName p tname)
  | tname == x = addTypeName p n >> pure t
  | otherwise  = pure n
-- In later stages, with all the typenames converted into rec types, we
-- just need to lookup upon the tenv to find the conversion
subsType tenv Nothing n@(TypeName p tname) = case tenv Map.!? tname of
  Just t ->
    addTypeName p n >> pure (changePos p (toTypeVar tname . toType $ snd t))
  Nothing -> pure n
-- In the first stage (converting typenames); we should ignore dualofs
subsType tenv Nothing n@(Dualof p t) = do
  addTypeName p n
  u <- subsType tenv Nothing t
  v <- dual u
  pure $ changePos p v
  -- addTypeName p n >> fmap (changePos p . dualFun) (subsType tenv Nothing t)
subsType _ _ t = pure t

-- Apply subsType over TypeMaps
subsMap :: TypeEnv -> Maybe (TypeVar, Type) -> TypeMap -> FreestState TypeMap
subsMap tenv b = mapM (subsType tenv b)

-- The notion of dual of is only for session types
-- we must apply it recursively on function types
-- TODO: Maybe to other type (functional) as well
-- dualFun :: Type -> Type
-- dualFun (Fun p pol t u) = Fun p pol (dualFun t) (dualFun u)
-- dualFun t               = dual t

-- Substitute expressions

subsExp :: TypeEnv -> Expression -> FreestState Expression
subsExp tenv (Abs p m b e) =
  liftM2 (Abs p m) (subsTypeBind tenv b) (subsExp tenv e)
subsExp tenv (App p e1 e2) = liftM2 (App p) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (Pair p e1 e2) =
  liftM2 (Pair p) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (BinLet p x y e1 e2) =
  liftM2 (BinLet p x y) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (Case p e m) =
  liftM2 (Case p) (subsExp tenv e) (subsFieldMap tenv m)
subsExp tenv (Conditional p e1 e2 e3) =
  liftM3 (Conditional p) (subsExp tenv e1) (subsExp tenv e2) (subsExp tenv e3)
subsExp tenv (TypeApp p x xs) =
  fmap (TypeApp p x) (mapM (subsType tenv Nothing) xs)
subsExp tenv (UnLet p x e1 e2) =
  liftM2 (UnLet p x) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (New p t u) =
  liftM2 (New p) (subsType tenv Nothing t) (subsType tenv Nothing u)
subsExp tenv (Select p x) = liftM (Select p) (pure x)
subsExp tenv (Match p e m) =
  liftM2 (Match p) (subsExp tenv e) (subsFieldMap tenv m)
subsExp _ e = return e

subsFieldMap :: TypeEnv -> FieldMap -> FreestState FieldMap
subsFieldMap tenv = mapM (\(ps, e) -> liftM2 (,) (pure ps) (subsExp tenv e))

subsTypeBind :: TypeEnv -> TypeBind -> FreestState TypeBind
subsTypeBind tenv (TypeBind p k t) = liftM (TypeBind p k) (subsType tenv Nothing t)


