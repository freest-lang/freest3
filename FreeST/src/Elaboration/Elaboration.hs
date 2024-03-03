{-# LANGUAGE LambdaCase, FlexibleInstances, BlockArguments , TypeFamilies, MonadComprehensions #-}

module Elaboration.Elaboration (elaboration, infer) where

import qualified Syntax.Base as T
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( isDatatypeContructor )
import qualified Syntax.Type as T
import qualified Elaboration.Phase as EP
import           Elaboration.Replace
import           Elaboration.ResolveDuality as Dual
import           Elaboration.ResolveEquations
import           Elaboration.Phase
import qualified Kinding.Subkind as SK (join)
import           Kinding.Kinding (synthetise)
import           Typing.Normalisation ( normalise )
import qualified Typing.Phase as VP
import qualified PatternMatch.Phase as PMP
import           Util.Error
import           Util.State

import           Inference.ConstraintKinding
import           Inference.ConstraintTyping
import           Inference.Unification
import           Inference.Phase
import           Syntax.Constraint

import           Data.Bifunctor
import           Data.Either

import           Control.Monad
import qualified Control.Monad.State as S
import           Data.Char (isLower)
import           Data.Functor hiding (void)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set


elaboration :: Set.Set Variable -> Set.Set Variable -> PMP.PatternS -> (VP.Defs, ElabS)
elaboration pkvs mvs patternS = S.runState elaboration' (patternToElab patternS)
  where
    patternToElab :: PMP.PatternS -> ElabS
    patternToElab s = s {ast = (ast s){definitions = definitions (ast s)}
                        , extra = EP.Extra{EP.mVariables=mvs, EP.pkVariables=pkvs} }
    
-- | 1. Solve the equations' system.
-- |    From this point, there are no type names on the function signatures
--      and on the function bodies. 
-- | 2. Resolve all the dualof occurrences on types (i.e. type A = dualof !Int)
--      From this point, there are no type names on the RHS
--      of the type declarations and datatypes (type env)
-- | 3. Substitute all type names on the function signatures
-- | 4. Same for parse env (which contains the functions' bodies)
-- | 5. Resolve all the dualof occurrences on signatures (i.e. f : dualof !Int -> Skip)
-- | 6. Resolve all the dualof occurrences on definitions (i.e. f c = send 5 c)
-- |    From this point there are no more occurrences of the dualof operator
-- | 7. Build the expression environment: substitute all
--      type operators on ExpEnv;
--      From f x = E and f : T -> U
--      build a lambda expression: f = \x : T -> E
elaboration' :: ElabState VP.Defs
elaboration'  = do
--  fixConsTypes
  getSignatures >>= quantifyPoly
  solveEquations
  getTypes >>= Dual.resolve >>= setTypes
  getSignatures >>= replaceSignatures
  getDefs >>= replaceDefinitions
  getSignatures >>= Dual.resolve >>= setSignatures
  getDefs >>= Dual.resolve >>= setDefs
  getDefs >>= buildDefs


  
  -- debugM . ("Program " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getProg
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getVEnv
  -- return defs



-- | Fix the multiplicity of the data constructor types
fixConsTypes :: InfState ()
fixConsTypes = do
  tys <- getTypes
  -- if this is the first step in the elaboration, there are still type names in
  -- signatures, so we need a non-empty kind environment. Empty env otherwise.
  let kEnv = Map.map fst tys
  getSignatures >>= tMapWithKeyM_ \k v -> S.when (isDatatypeContructor k tys)
    (fixConsType kEnv K.Un v >>= addToSignatures k)
  where
    fixConsType :: K.KindEnv -> K.Multiplicity -> T.Type -> InfState T.Type
    fixConsType kEnv m (T.Arrow s _ t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      T.Arrow s m t <$> fixConsType kEnv (SK.join m m') u
    fixConsType _ _ t = pure t

-- | Elaboration over environments (Signatures & Definitions)

replaceSignatures :: Signatures -> ElabState ()
replaceSignatures = tMapWithKeyM_ (\pv t -> addToSignatures pv {- . quantifyLowerFreeVars -} =<< replace t)
  -- where quantifyLowerFreeVars t = 
  --         foldr (\v t -> T.Forall p (T.Bind p v (K.ut p) t))
  --               t
  --               (Set.filter (isLower.head.show) $ T.free t)
  --         where p = getSpan t


replaceDefinitions :: Defs -> ElabState ()
replaceDefinitions = tMapWithKeyM_ (\x (ps, e) -> curry (addToDefinitions x) ps =<< replace e)

-- | Build a program from the parse env

buildDefs :: Defs -> ElabState VP.Defs 
buildDefs = Map.foldlWithKey (\def pv (ps,e) -> addToDefs def pv =<< buildFunBody pv ps e)
             (return Map.empty)
  where addToDefs acc pv e = acc >>= \def -> return $ Map.insert pv e def

buildFunBody :: Variable -> [Variable] -> E.Exp -> ElabState E.Exp
buildFunBody f as e = getFromSignatures f >>= \case
    Just s  -> buildExp e as s
    Nothing -> addError (FuctionLacksSignature (getSpan f) f) $> e
 where
  buildExp :: E.Exp -> [Variable] -> T.Type -> ElabState E.Exp
  buildExp e [] _ = pure e
  buildExp e bs t@(T.Rec _ _) = buildExp e bs (normalise t)
  buildExp e (b : bs) (T.Arrow _ m t1 t2) =
    E.Abs (getSpan b) m . Bind (getSpan b) b t1 <$> buildExp e bs t2
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    E.TypeAbs p . Bind p1 x k <$> buildExp e bs t
  buildExp _ _ t@(T.Dualof _ _) = internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp _ xs _ = do
    t <- fromJust <$> getFromSignatures f
    addError (WrongNumberOfArguments (getSpan f) f (length as - length xs) (length as) t) $> e


-- | Quantify polymorphic variables


quantifyPoly :: Signatures -> ElabState ()
quantifyPoly = tMapWithKeyM_ (\v t -> quantifyLowerFreeVars t >>= addToSignatures v)
  where
    quantifyLowerFreeVars t = 
      foldM (\t v -> freshKVar p >>= \k -> pure $ T.Forall p (T.Bind p v k t)) t
                (reverse $ Set.toList $ Set.filter (isLower . head . show) $ T.free t)
      where p = getSpan t




-- | Kind Inference

  
infer :: InfState ()
infer = do
  -- let var = mkVar defaultSpan "snd'"
  -- sigs <- getSignatures
  -- debugM $ show $ sigs Map.! var
  -- vars <- getMVariables
  
  getTypes >>= tMapM_ (\(k,t) -> cg Map.empty t)--  >>= \k' ->
                          -- addConstraint (KindC k' k))


  subs <- unify
  -- getTypes >>= setTypes . Map.map (bimap (subsKind subs) (subsOnType subs))
  -- getSignatures >>= setSignatures . Map.map (subsOnType subs)
  -- getDefs >>= setDefs . Map.map (subsOnExp subs)
  emptyConstraints
  
  s <- getSignatures
  getSignatures >>= tMapM_ (cg Map.empty)


  getDefs >>= tMapM_ (ctyping Map.empty)
  setSignatures s
  -- c <- getConstraints
  -- debugM $ show c

  subs' <- unify
  let subs1 = joinSubstitutions subs subs'
  -- debugM $ show subs1
  -- getSignatures >>= setSignatures . Map.map (subsOnType subs)
  -- getDefs >>= setDefs . Map.map (subsOnExp subs)
--  emptyConstraints
--  return ()
  getTypes >>= setTypes . Map.map (bimap (subsKind subs1) (subsOnType subs1))
  getSignatures >>= setSignatures . Map.map (subsOnType subs1)
  getDefs >>= setDefs . Map.map (subsOnExp subs1)
  fixConsTypes
  -- sigs <- getSignatures
  -- debugM $ show $ sigs Map.! var
--  getTypes >>= debugM . show . Map.filterWithKey (\k _ -> k == mkVar defaultSpan "T")

-- subsOnTypes :: Substitution


-- TODO: Type Class
subsKind :: Substitution -> K.Kind -> K.Kind
subsKind subs (K.Kind s (MultVar x) (K.PKVar y)) =
  K.Kind s (fromRight Lin $ subs Map.! x) (fromLeft K.Top $ subs Map.! y) 
subsKind subs (K.Kind s (MultVar y) pk) = K.Kind s (fromRight Lin $ subs Map.! y) pk
subsKind subs (K.Kind s m (K.PKVar y)) = K.Kind s m (fromLeft K.Top $ subs Map.! y)
subsKind _ k = k  


subsOnType :: Substitution -> T.Type -> T.Type
subsOnType subs (T.Arrow s m t1 t2) = T.Arrow s m (subsOnType subs t1) (subsOnType subs t2)
subsOnType subs (T.Labelled s sort m) = T.Labelled s sort (Map.map (subsOnType subs) m)
subsOnType subs (T.Semi s t1 t2) = T.Semi s (subsOnType subs t1) (subsOnType subs t2)
subsOnType subs (T.Message s p t) = T.Message s p (subsOnType subs t)
subsOnType subs (T.Forall s b) = T.Forall s (subsOnTBind subs b)
subsOnType subs (T.Rec s b) = T.Rec s (subsOnTBind subs b)
subsOnType subs (T.Dualof s t) = T.Dualof s (subsOnType subs t)
subsOnType _ t = t


subsOnTBind :: Substitution -> Bind K.Kind T.Type -> Bind K.Kind T.Type
subsOnTBind subs (Bind s x k t) = Bind s x (subsKind subs k) (subsOnType subs t)

subsOnEBind :: Substitution -> Bind T.Type E.Exp -> Bind T.Type E.Exp
subsOnEBind subs (Bind s x t e) = Bind s x (subsOnType subs t) (subsOnExp subs e)

subsOnKEBind :: Substitution -> Bind K.Kind E.Exp -> Bind K.Kind E.Exp
subsOnKEBind subs (Bind s x t e) = Bind s x (subsKind subs t) (subsOnExp subs e)


subsOnExp :: Substitution -> E.Exp -> E.Exp
subsOnExp subs (E.Abs s m b) = E.Abs s m (subsOnEBind subs b)
subsOnExp subs (E.App s e1 e2) = E.App s (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.Pair s e1 e2) = E.Pair s (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.BinLet s x y e1 e2) = E.BinLet s x y (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp subs (E.Case    s e fm) = E.Case s e (Map.map (second (subsOnExp subs)) fm)
subsOnExp subs (E.TypeAbs s b) = E.TypeAbs s (subsOnKEBind subs b) 
subsOnExp subs (E.TypeApp s e t) = E.TypeApp s (subsOnExp subs e) (subsOnType subs t) 
subsOnExp subs (E.UnLet s x e1 e2) = E.UnLet s x (subsOnExp subs e1) (subsOnExp subs e2)
subsOnExp _ e = e




-- showType :: T.Type -> String
-- showType (T.Int s) = "(Int)"
-- showType (T.Float s) = "(Float)"
-- showType (T.Char s) = "(Char)"
-- showType (T.String s) = "(String)"
-- showType (T.Skip s) = "(Skip)"
-- showType (T.End s p) = "(End)"
-- showType (T.Var s x) = "("++ show x++ ")"
-- showType (T.Arrow s m t1 t2) = "("++ showType t1 ++ " -> " ++ showType t2 ++ ")"
-- showType (T.Labelled s sort m) = "(Labelled "++ show sort ++ " " ++
--   "{" ++ Map.foldlWithKey (\str k v -> str ++ "( " ++ show k ++ ", " ++ showType v ++ "), " ) " " m ++ "})"
-- showType (T.Semi s t1 t2) = "("++ showType t1 ++ " ; " ++ showType t2 ++ ")"
-- showType (T.Message s p t) = "(MSG"++ show p ++ " " ++ showType t ++ ")"
-- showType (T.Forall s b) = "(FORALL "++ showBindT b ++ ")"
-- showType (T.Rec s b) = "(REC "++ showBindT b ++ ")"
-- showType (T.Dualof s t) = "(DUALOF "++ showType t ++ ")"

-- showBindT (Bind _ x k t)= "(BIND " ++ show x ++ " " ++ show k ++ " " ++ showType t ++ ")"


-- TODO: Try to break subs with a different TAbs and Forall (different vars) 

-- uniformType :: T.Type -> E.Exp -> E.Exp
-- uniformType (T.Arrow _ _ t1 t2) e =  uniformType t1 (uniformType t2 e) 
-- uniformType (T.Labelled _ _  m) e = Map.foldl (flip uniformType) e m
-- uniformType (T.Semi _ t1 t2) e = uniformType t1 (uniformType t2 e)
-- uniformType (T.Message _ _ t) e = uniformType t e
-- uniformType (T.Dualof _ t) e = uniformType t e
-- uniformType (T.Forall _ b) e = uniformType (body b) (findInExp (var b) (binder b) e)
--   -- T.Forall s (subsOnTBind subs b)
-- -- -- uniformType (T.Rec s b) e = T.Rec s (subsOnTBind subs b)

-- -- uniformType _ e = e

-- findInExp :: Variable -> K.Kind -> E.Exp -> E.Exp
-- findInExp a k (E.TypeAbs s (Bind s' x k' e))
--   | x == a = E.TypeAbs s $ Bind s' x k (findInExp a k e)
--   | otherwise = E.TypeAbs s $ Bind s' x k' (findInExp a k e)
-- findInExp a k (E.Abs s m (Bind s' x t e)) = E.Abs s m (Bind s' x t (findInExp a k e))
-- findInExp a k (E.App s e1 e2) = E.App s (findInExp a k e1) (findInExp a k e2)
-- findInExp a k (E.Pair s e1 e2) = E.Pair s (findInExp a k e1) (findInExp a k e2)
-- findInExp a k (E.BinLet s x y e1 e2) = E.BinLet s x y (findInExp a k e1) (findInExp a k e2)
-- findInExp a k (E.Case    s e fm) = E.Case s e (Map.map (second (findInExp a k)) fm)
-- findInExp a k (E.TypeApp s e t) = E.TypeApp s (findInExp a k e) t
-- findInExp a k (E.UnLet s x e1 e2) = E.UnLet s x (findInExp a k e1) (findInExp a k e2)
-- findInExp a k e = e
