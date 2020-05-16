module Validation.BuildTypes where

import Syntax.Expressions
import Syntax.Schemes
import Syntax.Types
import Syntax.Kinds
import Syntax.Base
import Syntax.Duality (dual)
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import Data.Map.Strict as Map
import qualified Data.Set as Set
import Utils.FreestState
import Debug.Trace
import Validation.Contractive
import Control.Monad
import           Utils.PreludeLoader (userDefined) -- debug

solveTypeDecls :: FreestState ()
solveTypeDecls = do
  tenv <- getTEnv
  let tenv' = typeDecls tenv
  traceM $ "\n1. INITIAL ENV: " ++ show tenv' ++ "\n"
  -- Solve the system of equations
  eqs <- solveEqs tenv'
  traceM $ "\n2. TYPENAMES: " ++ show eqs ++ "\n"
  -- Replace all occurrences of DualOf t
  eqs' <- solveDualOfs eqs
  traceM $ "\n3. DUALOFS: " ++ show eqs' ++ "\n"
  -- Check if the substituted types are contractive
  mapM_ (checkContractive Map.empty . snd) eqs'
  -- Substitute all type operators on VarEnv
  substituteVEnv eqs' 
  -- Substitute all type operators on ExpEnv
  substituteEEnv eqs'
  
-- PHASE 1: SOLVE THE SYSTEM OF EQUATIONS

solveEqs :: TypeEnv -> FreestState TypeEnv
solveEqs tenv = Map.foldlWithKey solveEq (return tenv) tenv
  where
    solveEq :: FreestState TypeEnv -> TypeVar -> (Kind, TypeScheme) -> FreestState TypeEnv
    solveEq acc x (k, s) = do
      let bt = buildRecursiveType x (k, s)
      acc' <- liftM (Map.insert x (k, (fromType bt))) acc -- TODO: update/alter
      substituteEnv x bt acc'
      
-- substitute every occurence of variable x in all the other entries of the map
substituteEnv :: TypeVar -> Type -> TypeEnv -> FreestState TypeEnv -- TODO: refactor
substituteEnv x t = tMapWithKeyM subsEnv
  where
    subsEnv :: TypeVar -> (Kind, TypeScheme) -> FreestState (Kind, TypeScheme)
    subsEnv v ks@(k, (TypeScheme p b s))
      | x == v    = pure ks -- ignore the node itself
      | otherwise = do     
          s' <- subs s
          let bt = buildRecursiveType v (k, (TypeScheme p b s'))
          return (k, TypeScheme p b bt)
          
    subs :: Type -> FreestState Type -- isn't this similar to the subsType? - treatment of Typenames
    subs (Fun p m t u)      = liftM2 (Fun p m) (subs t) (subs u)
    subs (PairType p t u)   = liftM2 (PairType p) (subs t) (subs u)
    subs (Datatype p m)     = liftM (Datatype p) (subsMap m)
    subs (Semi p t u)       = liftM2 (Semi p) (subs t) (subs u)
    subs (Choice p pol m)   = liftM (Choice p pol) (subsMap m)
    subs (Rec p tbs t)      = liftM (Rec p tbs) (subs t)
    subs (Dualof p t)       = liftM (Dualof p) (subs t)
    subs n@(TypeName p tname)
      | tname == x          = addTypeName p n >> return t
      | otherwise           = return n      
    subs n@(TypeVar p tname) -- should include type vars here?
      | tname == x          = addTypeName p n >> return t
      | otherwise           = return n
    subs s                  = return s

    subsMap :: TypeMap -> FreestState TypeMap
    subsMap = tMapM subs


-- GETTING ONLY TYPE DECLS FROM TENV (IGNORING DATATYPES)

typeDecls :: TypeEnv -> TypeEnv
typeDecls = Map.filter (\t -> not (isDataType (snd t)))

isDataType :: TypeScheme -> Bool
isDataType (TypeScheme _ _ (Datatype _ _)) = True
isDataType _                               = False

-- BUILDING RECURSIVE TYPES IF NEEDED

buildRecursiveType :: TypeVar -> (Kind, TypeScheme) -> Type
buildRecursiveType v (k, TypeScheme _ _ t)
  | isRecursiveTypeDecl v t =
      Rec (position v) (TypeVarBind (position v) v k) (toTypeVar t)
  | otherwise               = t

isRecursiveTypeDecl :: TypeVar -> Type -> Bool
isRecursiveTypeDecl v (Semi _ t u) = isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (Choice _ _ m) =
  Map.foldlWithKey (\b _ t -> b || isRecursiveTypeDecl v t) False m
isRecursiveTypeDecl v (Rec _ (TypeVarBind _ x _) t)-- TODO: recursion on t1
   | x == v    = False -- it is already a recursive type
   | otherwise = isRecursiveTypeDecl v t 
isRecursiveTypeDecl v (TypeName _ x)   = x == v
isRecursiveTypeDecl v (TypeVar _ x)    = x == v
isRecursiveTypeDecl v (Fun _ _ t u)    = isRecursiveTypeDecl v t || isRecursiveTypeDecl v u
isRecursiveTypeDecl v (PairType _ t u) = isRecursiveTypeDecl v t || isRecursiveTypeDecl v u 
isRecursiveTypeDecl v (Dualof _ t)     = isRecursiveTypeDecl v t
isRecursiveTypeDecl _ _                = False


-- TODO: find another solution to this? Or it is good? (in this case finish pattern matching)
toTypeVar :: Type -> Type
toTypeVar (Choice p pol m) = Choice p pol (Map.map toTypeVar m)
toTypeVar (TypeName p x)   = TypeVar p x
toTypeVar (Semi p t1 t2)   = Semi p (toTypeVar t1) (toTypeVar t2)
toTypeVar (Rec p xs t)     = Rec p xs (toTypeVar t)
toTypeVar (Fun p m t u)    = Fun p m (toTypeVar t) (toTypeVar u)
toTypeVar (PairType p t u) = PairType p (toTypeVar t) (toTypeVar u)
toTypeVar t                = t

-- PHASE 2 - SOLVING DUALOF TYPE OPERATORS

solveDualOfs :: TypeEnv -> FreestState TypeEnv
solveDualOfs tenv = 
  tMapM (\(k, TypeScheme p xs t) ->
            solveDualOf tenv (Set.insert t Set.empty) t >>=
            \t' -> return (k, TypeScheme p xs t')) tenv

type Visited = Set.Set Type
-- TODO: update recursion variable (name) on dual ?
-- TODO: more p matching??
-- TODO: type becomes unfolded when expanding dualofs
-- TODO: fauns and all the remaining constructors
solveDualOf :: TypeEnv -> Visited -> Type -> FreestState Type
solveDualOf tenv s (Choice p pol m) = liftM (Choice p pol) (tMapM (solveDualOf tenv s) m)
solveDualOf tenv s (Semi p t u)     = liftM2 (Semi p) (solveDualOf tenv s t) (solveDualOf tenv s u)    
solveDualOf tenv s (Rec p xs t)     = liftM (Rec p xs) (solveDualOf tenv s t)   
solveDualOf tenv s (Fun p pol t u)  = liftM2 (Fun p pol) (solveDualOf tenv s t) (solveDualOf tenv s u)
solveDualOf tenv s t@(TypeName p tname) = -- TODO: refactor
  case tenv Map.!? tname of
    Just (_, TypeScheme p b t) -> pure (toTypeVar t)
    Nothing -> maybeErr t             
solveDualOf tenv s d@(Dualof p t) = do
  addTypeName p d
  liftM dual (solveDualOf tenv s t)
solveDualOf _ _ p = return p


-- Until we have datatypes as typenames; we have to check if it is a datatype
-- In other words we have to check if it is on the original map
-- TODO: When typenames become rec types as well keep only the Nothing case
maybeErr :: Type -> FreestState Type
maybeErr (TypeName p tname) = do
  tenv <- getTEnv
  case tenv Map.!? tname of
    Just (_, TypeScheme _ _ t) -> pure t
    Nothing -> do 
      addError p [Error "Type name not in scope:", Error tname]
      return (Basic p UnitType) -- TODO: should return t (typename) or a unit type??
      
-- TODO : Worth it?
-- Yes, but only on top-level... Complete
changePos :: Pos -> Type -> Type -- rec call???
changePos p (Rec _ xs t) = (Rec p xs (changePos p t))
changePos p (Semi _ t u) = Semi p t u
changePos p (PairType _ t u) = PairType p t u
changePos p (Basic _ t) = Basic p t
changePos p (Fun _ pol t u) = Fun p pol t u
changePos _ t = t

  
-- PHASE 3: SUBSTITUTE ON FUNCTION SIGNATURES (VARENV)

-- twice
substituteVEnv :: TypeEnv -> FreestState ()
substituteVEnv tenv = do
  venv <- getVEnv
  tMapWithKeyM_ subsElem venv
  where
    subsElem :: ProgVar -> TypeScheme -> FreestState ()
    subsElem pv (TypeScheme p b s) = do
      s' <- subsType tenv s
      addToVEnv pv (TypeScheme p b s')

-- TODO: maybe refactor and use substitute env
subsType :: TypeEnv -> Type -> FreestState Type
subsType tenv (Fun p m t1 t2)      = liftM2 (Fun p m) (subsType tenv t1) (subsType tenv t2)
subsType tenv (PairType p t1 t2)   = liftM2 (PairType p) (subsType tenv t1) (subsType tenv t2)
subsType tenv (Datatype p m)       = liftM (Datatype p) (mapM (subsType tenv) m)
subsType tenv (Semi p t1 t2)       = liftM2 (Semi p) (subsType tenv t1) (subsType tenv t2)
subsType tenv (Choice p pol m)     = liftM (Choice p pol) (mapM (subsType tenv) m)
subsType tenv (Rec p tvb t1)       = liftM (Rec p tvb) (subsType tenv t1)
subsType tenv n@(TypeName p tname) =   
  case tenv Map.!? tname of
    Just t  -> addTypeName p n >> pure (changePos p (toTypeVar $ toType $ snd t))
    Nothing -> pure n
subsType tenv n@(Dualof p t)       =
  addTypeName p n >> liftM (changePos p . dualFun) (subsType tenv t)
subsType _ t                       = pure t


dualFun :: Type -> Type
dualFun (Fun p pol t u) = Fun p pol (dualFun t) (dualFun u)
dualFun t = dual t

-- PHASE 4: SUBSTITUTE TYPES ON THE EXPRESSIONS (EXPENV)

-- substitute venv
-- for each element substitute the type
-- and update venv
-- Also, inserts in the acc Position |- original type
substituteEEnv :: TypeEnv -> FreestState ()
substituteEEnv tenv = getEEnv >>= \eenv -> tMapWithKeyM_ subsUpdateExp eenv
  where
    subsUpdateExp :: ProgVar -> Expression -> FreestState ()
    subsUpdateExp pv e = do
      e' <- subsExp tenv e
      addToEEnv pv e'

subsExp :: TypeEnv -> Expression -> FreestState Expression
subsExp tenv (Lambda p m pv t e)  = liftM2 (Lambda p m pv) (subsType tenv t) (subsExp tenv e)
subsExp tenv (App p e1 e2)        = liftM2 (App p) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (Pair p e1 e2)       = liftM2 (Pair p) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (BinLet p x y e1 e2) = liftM2 (BinLet p x y) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (Case p e m)         = liftM2 (Case p) (subsExp tenv e) (subsFieldMap tenv m)
subsExp tenv (Conditional p e1 e2 e3) = liftM3 (Conditional p) (subsExp tenv e1)
                                           (subsExp tenv e2) (subsExp tenv e3) 
subsExp tenv (TypeApp p x xs)  = liftM (TypeApp p x) (mapM (subsType tenv) xs)
subsExp tenv (UnLet p x e1 e2) = liftM2 (UnLet p x) (subsExp tenv e1) (subsExp tenv e2)
subsExp tenv (Fork p e)        = liftM (Fork p) (subsExp tenv e)
subsExp tenv (New p t u)       = liftM2 (New p) (subsType tenv t) (subsType tenv u)
subsExp tenv (Send p e)        = liftM (Send p) (subsExp tenv e)
subsExp tenv (Receive p e)     = liftM (Receive p) (subsExp tenv e)
subsExp tenv (Select p e x)    = liftM2 (Select p) (subsExp tenv e) (pure x)
subsExp tenv (Match p e m)     = liftM2 (Match p) (subsExp tenv e) (subsFieldMap tenv m)
subsExp _ e                    = return e

subsFieldMap :: TypeEnv -> FieldMap -> FreestState FieldMap
subsFieldMap tenv = mapM (\(ps, e) -> liftM2 (,) (pure ps) (subsExp tenv e))
