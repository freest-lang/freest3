{-# LANGUAGE LambdaCase #-}
module Validation.Elaboration
  ( elaborateTypes
  )
where

import           Syntax.Expression
-- import           Syntax.Schemes
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Syntax.Base
import           Syntax.TypeVariable
import           Syntax.ProgramVariable
import           Validation.Duality             ( dual )
-- import           Validation.Terminated
import           Utils.FreestState
import           Data.Map.Strict               as Map
-- import qualified Data.Set                      as Set
-- import           Debug.Trace
import           Control.Monad                  ( -- liftM,
                                                  liftM2
                                                , liftM3
                                                )
-- import           Utils.PreludeLoader            ( userDefined ) -- debug
import           Validation.Kinding             ( synthetise )


elaborateTypes :: FreestState ()
elaborateTypes = do
  tenv <- getTEnv
 -- let tenv' = typeDecls tenv

--  debugM $ "INITIAL ENV: " ++ show tenv
  -- Solve the system of equations
  eqs  <- solveEqs tenv

  -- debugM $ "TYPENAMES: " ++ show eqs
  -- Replace all occurrences of DualOf t
  eqs' <- solveDualOfs eqs
--  let eqs' = eqs
--  debugM $ "DUALOFS: " ++ show eqs' ++ "\n"

  -- Check if the substituted types are contractive  
  mapM_ (synthetise Map.empty . snd) eqs'
--  mapM_ (checkContractive Map.empty . snd) eqs'

--  venv <- getVEnv
--  debugM $ "BEFORE VENV: " ++ show (userDefined venv) ++ "\n"

  -- Substitute all type operators on VarEnv
  substituteVEnv eqs'

--  venv <- getVEnv
--  debugM $ "AFTER VENV: " ++ show (userDefined venv) ++ "\n"

  -- Substitute all type operators on ExpEnv

--  eenv <- getEEnv
--  debugM $ "BEFORE EENV: " ++ show eenv ++ "\n"

  substituteEEnv eqs'
--  eenv <- getEEnv
--  debugM $ "AFTER EENV: " ++ show eenv ++ "\n"

-- PHASE 1: SOLVE THE SYSTEM OF EQUATIONS

solveEqs :: T.TypeEnv -> FreestState T.TypeEnv
solveEqs tenv = Map.foldlWithKey solveEq (return tenv) tenv
 where
  solveEq
    :: FreestState T.TypeEnv
    -> TypeVar
    -> (K.Kind, T.Type)
    -> FreestState T.TypeEnv
  solveEq acc x t = do
    let bt = buildRecursiveType x t
    -- fmap (Map.insert x (fst t, bt)) acc >>=
    acc' <- acc
    substituteEnv x (fst t, bt) acc'
   -- acc' <- acc
   -- let bt = buildRecursiveType x t
   -- substituteEnv x (fst t, bt) acc'
    -- i <- getNextIndex
    -- traceM $ "------------------------------\n"
    --        ++ show i ++ " - initial " ++ show accBef
    --        ++ "\n\nEnv\t" ++ show tmp
    --        ++ "\n------------------------------\n\n"
    -- return tmp


-- substitute every occurence of variable x in all the other entries of the map
-- substituteEnv :: TypeVar -> Type -> TypeEnv -> FreestState TypeEnv
-- substituteEnv x t = tMapWithKeyM subsEnv
--  where
--   subsEnv :: TypeVar -> (K.Kind, Type) -> FreestState (K.Kind, Type)
--   subsEnv v ks@(k, s)
--     | x == v = pure ks -- ignore the node itself
--     | otherwise = do
--        -- traceM $ "SubsEnv. Substituing  " ++ show v ++ " with " ++ show s
--         s' <- subsType Map.empty (Just (x, t)) s
--         return (k, buildRecursiveType v (k, s'))

substituteEnv
  :: TypeVar -> (K.Kind, T.Type) -> T.TypeEnv -> FreestState T.TypeEnv
substituteEnv x t tenv = do -- tMapWithKeyM subsEnv
  -- debugM ("Subs " ++ show x ++ ":\n" ++ show tenv)
  tmp <- tMapWithKeyM subsEnv tenv
  -- debugM ("After subs " ++ show x ++ ":\n" ++ show tmp)
  pure tmp
 where
  subsEnv :: TypeVar -> (K.Kind, T.Type) -> FreestState (K.Kind, T.Type)
  subsEnv v ks@(k, s)
    | x == v = pure (k, buildRecursiveType v (k, s))
    | -- ignore the node itself
      otherwise = do
       -- traceM $ "SubsEnv. Substituing  " ++ show v ++ " with " ++ show s
--        let bt = buildRecursiveType x t
      s' <- subsType Map.empty (Just (x, snd t)) s

      -- debugM $ "------------------------------\n"
      --    ++ "Subs typevar\t" ++ show x ++ "\nwith type:\t" ++ show t
      --    ++ "\non var:         " ++ show v ++ "\nwith type:\t" ++ show s
      --    ++ "\nresult:         " ++ show s' ++ "\n"
      --    ++ "\n\nTREESTACK " ++ show tenv
      --    ++ "\n------------------------------\n\n"
      return (k, buildRecursiveType v (k, s'))


-- GETTING ONLY TYPE DECLS FROM TENV (IGNORING DATATYPES)

-- typeDecls :: TypeEnv -> TypeEnv
-- typeDecls = Map.filter (not . isDataType . snd )

-- isDataType :: Type -> Bool
-- isDataType (Datatype _ _) = True
-- isDataType _              = False

-- BUILDING RECURSIVE TYPES IF NEEDED

buildRecursiveType :: TypeVar -> (K.Kind, T.Type) -> T.Type
buildRecursiveType v (k, t)
  | isRecursiveTypeDecl v t = T.Rec (pos v) (K.Bind (pos v) v k) t
  | otherwise               = t

isRecursiveTypeDecl :: TypeVar -> T.Type -> Bool
isRecursiveTypeDecl v (T.Semi _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (T.Choice _ _ m) =
  Map.foldlWithKey (\b _ t -> b || isRecursiveTypeDecl v t) False m
isRecursiveTypeDecl v (T.Datatype _ m) =
  Map.foldlWithKey (\b _ t -> b || isRecursiveTypeDecl v t) False m
isRecursiveTypeDecl v (T.Rec _ (K.Bind _ x _) t)
  | x == v    = False
  | -- it is already a recursive type
    otherwise = isRecursiveTypeDecl v t
isRecursiveTypeDecl v (T.Forall _ _ t) = isRecursiveTypeDecl v t
isRecursiveTypeDecl v (T.TypeName _ x) = x == v
isRecursiveTypeDecl v (T.TypeVar  _ x) = x == v
isRecursiveTypeDecl v (T.Fun _ _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (T.Pair _ t u) =
  isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (T.Dualof _ t) = isRecursiveTypeDecl v t
isRecursiveTypeDecl _ _              = False


-- Convert typenames to typeVars; when a type declaration is converted in a rec type
-- Added the additional type var because datatypes are also typenames
-- toTypeVar :: TypeVar -> Type -> Type
-- toTypeVar x (Choice p pol m) = Choice p pol (Map.map (toTypeVar x) m)
-- toTypeVar x (TypeName p tname) | --
--                                  x == tname = TypeVar p tname
--                                | otherwise  = TypeName p tname
-- toTypeVar x (Semi p t1 t2) = Semi p (toTypeVar x t1) (toTypeVar x t2)
-- toTypeVar _ (Rec p xs@(K.Bind _ x _) t) = Rec p xs (toTypeVar x t)
-- toTypeVar x (Forall p kb t) = Forall p kb (toTypeVar x t)
-- -- functional types
-- toTypeVar x (Fun p m t u) = Fun p m (toTypeVar x t) (toTypeVar x u)
-- toTypeVar x (Pair p t u) = Pair p (toTypeVar x t) (toTypeVar x u)
-- -- Datatype
-- toTypeVar _ t = t

-- PHASE 2 - SOLVING DUALOF TYPE OPERATORS

solveDualOfs :: T.TypeEnv -> FreestState T.TypeEnv
solveDualOfs tenv =
  tMapM (\(k, t) -> solveDualOf tenv False t >>= \t' -> pure (k, t')) tenv

solveDualOf :: T.TypeEnv -> Bool -> T.Type -> FreestState T.Type
solveDualOf tenv b (T.Choice p pol m) =
  fmap (T.Choice p pol) (tMapM (solveDualOf tenv b) m)
solveDualOf tenv b (T.Semi p t u) =
  liftM2 (T.Semi p) (solveDualOf tenv b t) (solveDualOf tenv b u)
solveDualOf tenv b (T.Rec p xs t) = fmap (T.Rec p xs) (solveDualOf tenv b t)
solveDualOf tenv b (T.Forall p kb t) =
  fmap (T.Forall p kb) (solveDualOf tenv b t)
solveDualOf tenv b (T.Fun p pol t u) =
  liftM2 (T.Fun p pol) (solveDualOf tenv b t) (solveDualOf tenv b u)
-- solveDualOf tenv n@(TypeName _ tname) = case tenv Map.!? tname of
--   Just (_, t) -> pure (toTypeVar tname t)
--   Nothing     -> maybeScopeErr n
solveDualOf tenv b n@(T.TypeVar _ tname)
  | b = case tenv Map.!? tname of
    Just (_, t) -> pure t
    Nothing     -> maybeScopeErr n
  | otherwise = pure n
solveDualOf tenv _ d@(T.Dualof p t) = do
  addTypeName p d
  u <- solveDualOf tenv True t
  dual u
--  fmap dual (solveDualOf tenv t)
-- solveDualOf visited tenv t@(TypeVar p x)
--   | x `Set.member` visited = return t
--   | otherwise = return $ Dualof p t
-- solveDualOf _ _ t = return t

--  fmap dual (solveDualOf tenv t)
solveDualOf _ _ p = return p


-- dualType :: TypeEnv -> Type -> FreestState Type
-- dualType tenv n@(TypeVar _ x) = 
--   case tenv Map.!? x of
--     Just (_, t) -> pure t
--     Nothing     -> maybeScopeErr n
-- dualType _ t = dual t    


-- Until we have datatypes as typenames; we have to filter non-datatypes
-- In other words we have to check if it is on the original map
-- TODO: When datatypes become recursive types as well; one should keep
-- only the Nothing (error) case ?
maybeScopeErr :: T.Type -> FreestState T.Type
maybeScopeErr (T.TypeVar p tname) = getFromTEnv tname >>= \case
  Just (_, t) -> pure t
  Nothing     -> addError p [Error "Type name not in scope:", Error tname]
    >> pure (T.UnitType p)

-- Change position of a given type with a given position
changePos :: Pos -> T.Type -> T.Type
changePos p (T.IntType  _     ) = T.IntType p
changePos p (T.CharType _     ) = T.CharType p
changePos p (T.BoolType _     ) = T.BoolType p
changePos p (T.UnitType _     ) = T.UnitType p
changePos p (T.Fun _ pol t u  ) = T.Fun p pol t u
changePos p (T.Pair    _ t   u) = T.Pair p t u
-- Datatype
-- Skip
changePos p (T.Semi    _ t   u) = T.Semi p t u
changePos p (T.Message _ pol b) = T.Message p pol b
changePos p (T.Choice  _ pol m) = T.Choice p pol m
changePos p (T.Rec     _ xs  t) = T.Rec p xs t -- (changePos p t)
changePos p (T.Forall  _ xs  t) = T.Forall p xs t -- (changePos p t)
-- TypeVar
changePos _ t                   = t


-- PHASE 3: SUBSTITUTE ON FUNCTION SIGNATURES (VARENV)

substituteVEnv :: T.TypeEnv -> FreestState ()
substituteVEnv tenv = getVEnv >>= tMapWithKeyM_ subsElem
 where
  subsElem :: ProgVar -> T.Type -> FreestState ()
  subsElem pv s = do
    s' <- subsType tenv Nothing s
    addToVEnv pv s'

-- PHASE 4: SUBSTITUTE TYPES ON THE EXPRESSIONS (EXPENV)

substituteEEnv :: T.TypeEnv -> FreestState ()
substituteEEnv tenv = getEEnv >>= \eenv -> tMapWithKeyM_ subsUpdateExp eenv
 where
  subsUpdateExp :: ProgVar -> Exp -> FreestState ()
  subsUpdateExp pv e = subsExp tenv e >>= \e1 -> addToEEnv pv e1


-- SUBSTITUTIONS

-- Substitute a type

subsType :: T.TypeEnv -> Maybe (TypeVar, T.Type) -> T.Type -> FreestState T.Type
subsType tenv b (T.Fun p m t1 t2) =
  liftM2 (T.Fun p m) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b (T.Pair p t1 t2) =
  liftM2 (T.Pair p) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b d@(T.Datatype p m) = do
 -- m' <- subsMap tenv b m
  -- traceM $ "\nSubs Datatype -> " ++show d ++ "\nMaybe: " ++ show b ++ "\nBEFORE: " ++ show m' ++ "\nAFTER:  " ++ show m ++ "\n"
  fmap (T.Datatype p) (subsMap tenv b m)
subsType tenv b (T.Semi p t1 t2) =
  liftM2 (T.Semi p) (subsType tenv b t1) (subsType tenv b t2)
subsType tenv b (T.Choice p pol m ) = fmap (T.Choice p pol) (subsMap tenv b m)
subsType tenv b (T.Forall p kb  t ) = fmap (T.Forall p kb) (subsType tenv b t)
subsType tenv b (T.Rec    p tvb t1) = fmap (T.Rec p tvb) (subsType tenv b t1)
-- In the first phase, we only substitute if the typename is the one that
-- we are looking for (x)
subsType _ (Just (x, t)) n@(T.TypeVar p tname)
  | -- n@(TypeName p tname)
    tname == x = {- traceM ("substituing " ++ show n) >> -}
                 addTypeName p n >> pure t
  | otherwise  = pure n
-- In later stages, with all the typenames converted into rec types, we
-- just need to lookup upon the tenv to find the conversion
subsType tenv Nothing n@(T.TypeVar p tname) = case tenv Map.!? tname of
  Just t  -> addTypeName p n >> pure (changePos p (snd t))--(toTypeVar tname (snd t)))
  Nothing -> pure n
-- In the first stage (converting typenames); we should ignore dualofs
subsType tenv Nothing n@(T.Dualof p t) = do
  addTypeName p n
  u <- subsType tenv Nothing t
  v <- dual u
  pure $ changePos p v
  -- addTypeName p n >> fmap (changePos p . dualFun) (subsType tenv Nothing t)
subsType _ _ t = pure t

-- Apply subsType over TypeMaps
subsMap
  :: T.TypeEnv -> Maybe (TypeVar, T.Type) -> T.TypeMap -> FreestState T.TypeMap
subsMap tenv b = mapM (subsType tenv b)

-- The notion of dual of is only for session types
-- we must apply it recursively on function types
-- TODO: Maybe to other type (functional) as well
-- dualFun :: Type -> Type
-- dualFun (Fun p pol t u) = Fun p pol (dualFun t) (dualFun u)
-- dualFun t               = dual t

-- Substitute expressions

subsExp :: T.TypeEnv -> Exp -> FreestState Exp
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
subsExp tenv (TypeAbs p x e) = fmap (TypeAbs p x) (subsExp tenv e)
subsExp tenv (TypeApp p e t) =
  liftM2 (TypeApp p) (subsExp tenv e) (subsType tenv Nothing t) -- (mapM (subsType tenv Nothing) xs)
subsExp tenv (UnLet p x e1 e2) =
  liftM2 (UnLet p x) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (New p t u) =
  liftM2 (New p) (subsType tenv Nothing t) (subsType tenv Nothing u)
subsExp _ (Select p x) = fmap (Select p) (pure x)
subsExp tenv (Match p e m) =
  liftM2 (Match p) (subsExp tenv e) (subsFieldMap tenv m)
subsExp _ e = return e

subsFieldMap :: T.TypeEnv -> FieldMap -> FreestState FieldMap
subsFieldMap tenv = mapM (\(ps, e) -> liftM2 (,) (pure ps) (subsExp tenv e))

subsTypeBind :: T.TypeEnv -> T.Bind -> FreestState T.Bind
subsTypeBind tenv (T.Bind p k t) = fmap (T.Bind p k) (subsType tenv Nothing t)
