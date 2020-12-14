{-|
Module      :  Validation.Typing
Description :  Checking the good formation of expressions
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

A bidirectional type system.
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

-- TODO: remove NoMonadFailDesugaring and add an instance monad fail
module Validation.Typing
  ( synthetise
  , checkAgainst
  , fillFunType
  )
where

import           Syntax.Base
import qualified Syntax.Kind                   as K
import qualified Syntax.Type                   as T
import qualified Syntax.Expression             as E
import           Syntax.ProgramVariable
import           Parse.Unparser -- debug
import           Validation.Extract
import qualified Validation.Kinding            as K -- Again?
import qualified Validation.Rename             as Rename
                                                ( subs )
import           Equivalence.Equivalence
import           Utils.FreestState
import           Utils.PreludeLoader            ( userDefined ) -- debug
import           Control.Monad.State            ( when
                                                , unless
                                                )
import qualified Data.Map.Strict               as Map

-- SYNTHESISING A TYPE

synthetise :: K.KindEnv -> E.Exp -> FreestState T.Type
-- Basic expressions
synthetise _ (E.Int p _) = return $ T.Int p
synthetise _ (E.Char p _) = return $ T.Char p
synthetise _ (E.Bool p _) = return $ T.Bool p
synthetise _ (E.Unit p) = return $ T.Unit p
-- Variable
synthetise kEnv e@(E.Var p x)
  | x == mkVar p "receive" = addPartiallyAppliedError e "channel"
  | x == mkVar p "send" = addPartiallyAppliedError
    e
    "value and another denoting a channel"
  | otherwise = synthetiseVar kEnv x
  -- TODO: Error message
--      debugM $ show x    
      -- s@(TypeScheme _ bs t) <- synthetiseVar kEnv x
      -- unless
      --   (null bs)
      --   (addError
      --     p
      --     [ Error "Variable"
      --     , Error x
      --     , Error "of a polymorphic type used in a monomorphic context\n"
      --     , Error "\t The type scheme for variable"
      --     , Error x
      --     , Error "is"
      --     , Error s
      --     , Error "\n\t Consider instantiating variable"
      --     , Error x
      --     , Error "with appropriate types for the polymorphic variables"
      --     ]
      --   )
      -- return t
synthetise kEnv (E.UnLet _ x e1 e2) = do
  t1 <- synthetise kEnv e1
  addToVEnv x t1
  t2 <- synthetise kEnv e2
  quotient kEnv x
  return t2
-- Abs introduction
synthetise kEnv e'@(E.Abs p m (T.Bind _ x t1) e) = do
  K.synthetise kEnv t1
  vEnv1 <- getVEnv
  addToVEnv x t1
  t2 <- synthetise kEnv e
  quotient kEnv x
  vEnv2 <- getVEnv
  when (m == Un) (checkEqualEnvs e' vEnv1 vEnv2)
  return $ T.Fun p m t1 t2
-- Abs elimination
synthetise kEnv (E.App p (E.Select _ c) e) = do
  t <- synthetise kEnv e
  m <- extractOutChoiceMap e t
  extractCons p m c
synthetise kEnv (E.App p (E.Var _ x) e) |  -- Receive e
                                          x == mkVar p "receive" = do
  t        <- synthetise kEnv e
  (u1, u2) <- extractInput e t
  K.checkAgainst kEnv (K.ml (pos u1)) u1
  return $ T.Pair p u1 u2
synthetise kEnv e@(E.App p (E.Var _ x) _) |  -- Send e
                                            x == mkVar p "send" =
  addPartiallyAppliedError e "channel"
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) |  -- Send e1 e2
                                                      x == mkVar p "send" = do
  t        <- synthetise kEnv e2
  (u1, u2) <- extractOutput e2 t
  K.checkAgainst kEnv (K.ml (pos u1)) u1
  checkAgainst kEnv e1 $ u1
  return u2
synthetise kEnv e@(E.App _ e1 e2) = do -- General case
--  debugM ("Extract Fun: " ++ show e)
  t        <- synthetise kEnv e1
  (u1, u2) <- extractFun e1 t
--  debugM ("Extract Fun: " ++ show u1 ++ " | "  ++ show u2 ++ " against " ++ show e2)
  checkAgainst kEnv e2 u1
  return u2

  -- Type Abstraction intro and elim

--        ∆, a : κ | Γ |- e : T
----------------------------------------
--   ∆ | Γ |- Λa : κ.e : ∀ a : κ . T

synthetise kEnv (E.TypeAbs p kb@(K.Bind p' x k) e) = do
--  addToVEnv x (TypeVar p' x)
  t <- synthetise (Map.insert x k kEnv) e
  return $ T.Forall p kb t

-- synthetise kEnv e'@(Abs p m (TypeBind _ x t1) e) = do
--   K.synthetise kEnv t1
--   vEnv1 <- getVEnv
--   addToVEnv x t1
--   t2 <- synthetise kEnv e
--   quotient kEnv x
--   vEnv2 <- getVEnv
--   when (m == Un) (checkEqualEnvs e' vEnv1 vEnv2)
--   return $ Fun p m t1 t2

-- Type application
--  ∆ | Γ |- e : ∀ a : κ . U        ∆ |- T : κ
-------------------------------------------------
--     ∆ | Γ |- e T : [T /a]U

synthetise kEnv (E.TypeApp p e t) = do -- TODO: error and bs, zip
  u                              <- synthetise kEnv e
  (T.Forall _ (K.Bind _ y _) u') <- extractForall e u
  K.synthetise kEnv t
  -- let tmp = Rename.subs t y u'
  -- traceM $ "\ESC[91m"
  --   ++ "TYPEAPP ->\n\te: " ++ show e
  --   ++ "\n\tu: "  ++ show u
  --   ++ "\n\tu': "  ++ show u'
  --   ++ "\n\ty: " ++ show y
  --   ++ "\n\tt: " ++ show t
  --   ++ "\n\tsubs " ++ show tmp
  --   ++ "\ESC[0m" 
  return $ Rename.subs t y u'
  -- when
  --   (length ts /= length bs)
  --   (addError
  --     p
  --     [ Error "Wrong number of arguments to type application\n"
  --     , Error "\t parameters:"
  --     , Error bs
  --     , Error "\n\t arguments: "
  --     , Error ts
  --     ]
  --   )
  -- let typeKinds = [] -- zip ts bs :: [(Type, KindBind)]
  -- mapM_ (\(u, KindBind _ _ k) -> K.checkAgainst kEnv k u) typeKinds
  -- return $ foldr (\(u, KindBind _ y _) -> Rename.subs u y) t typeKinds
-- Boolean elimination
synthetise kEnv (E.Conditional p e1 e2 e3) = do
  checkAgainst kEnv e1 (T.Bool p)
  vEnv2 <- getVEnv
  t     <- synthetise kEnv e2
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p "conditional" kEnv vEnv3 vEnv4
  return t
-- Pair introduction
synthetise kEnv (E.Pair p e1 e2) = do
  t1 <- synthetise kEnv e1
  t2 <- synthetise kEnv e2
  -- K.checkAgainst kEnv (Kind p Functional m) t1
  -- K.checkAgainst kEnv (Kind p Functional m) t2
  return $ T.Pair p t1 t2
-- Pair elimination
synthetise kEnv (E.BinLet _ x y e1 e2) = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- extractPair e1 t1
  addToVEnv x u1
  addToVEnv y u2
  vEnv <- getVEnv
  t2   <- synthetise kEnv e2
  quotient kEnv x
  quotient kEnv y
  return t2
-- Datatype elimination
synthetise kEnv (E.Case p e fm) =
  synthetiseFieldMap p "case" kEnv e fm extractDatatypeMap paramsToVEnvCM
-- Session types
-- New
synthetise kEnv (E.New p t u) = do
  K.checkAgainstSession kEnv t
  return $ T.Pair p t u -- (dual t)
synthetise kEnv e@(E.Select _ _) = addPartiallyAppliedError e "channel"
-- Match
synthetise kEnv (E.Match p e fm) =
  synthetiseFieldMap p "match" kEnv e fm extractInChoiceMap paramsToVEnvMM

-- | Returns the type scheme for a variable; removes it from vEnv if lin
synthetiseVar :: K.KindEnv -> ProgVar -> FreestState T.Type
synthetiseVar kEnv x = getFromVEnv x >>= \case
  Just s -> do
    k <- K.synthetise kEnv s
    when (K.isLin k) $ removeFromVEnv x
    return s
  Nothing -> do
    let p = pos x
    addError
      p
      [ Error "Variable or data constructor not in scope:"
      , Error x
      , Error "\n\t (is"
      , Error x
      , Error "a linear variable that has been consumed?)"
      ]
    let s = omission p
    addToVEnv x s
    return s

synthetiseFieldMap
  :: Pos
  -> String
  -> K.KindEnv
  -> E.Exp
  -> E.FieldMap
  -> (E.Exp -> T.Type -> FreestState T.TypeMap)
  -> (ProgVar -> [ProgVar] -> T.Type -> FreestState ())
  -> FreestState T.Type
synthetiseFieldMap p branching kEnv e fm extract params = do
  t  <- synthetise kEnv e
  tm <- extract e t
  if Map.size fm /= Map.size tm
    then do
      addError
        p
        [ Error "Wrong number of constructors\n"
        , Error "\t The expression has"
        , Error $ Map.size fm
        , Error "constructor(s)\n"
        , Error "\t but the type has"
        , Error $ Map.size tm
        , Error "constructor(s)\n"
        , Error "\t in case/match"
--        , Error $ "\ESC[91m" ++ showFieldMap 1 fm ++ "\ESC[0m"
        , Error $ "\ESC[91m{" ++ showFieldMap fm ++ "}\ESC[0m"
        ]
      return $ omission p
    else do
      vEnv             <- getVEnv
      (t : ts, v : vs) <- Map.foldrWithKey
        (synthetiseField vEnv kEnv params tm)
        (return ([], []))
        fm
      mapM_ (checkEquivTypes e kEnv t)          ts
      mapM_ (checkEquivEnvs p branching kEnv v) vs
      setVEnv v
      return t

-- Checks either the case map and the match map (all the expressions)
synthetiseField
  :: T.VarEnv
  -> K.KindEnv
  -> (ProgVar -> [ProgVar] -> T.Type -> FreestState ())
  -> T.TypeMap
  -> ProgVar
  -> ([ProgVar], E.Exp)
  -> FreestState ([T.Type], [T.VarEnv])
  -> FreestState ([T.Type], [T.VarEnv])
synthetiseField vEnv1 kEnv params tm b (bs, e) state = do
  (ts, vEnvs) <- state
  setVEnv vEnv1
  t1 <- synthetiseCons b tm
  params b bs t1
  t2 <- fillFunType kEnv b e t1
  mapM_ (quotient kEnv) bs
  vEnv2 <- getVEnv
  return (t2 : ts, vEnv2 : vEnvs)

-- match map
paramsToVEnvMM :: ProgVar -> [ProgVar] -> T.Type -> FreestState ()
paramsToVEnvMM c bs t = do
  addToVEnv (head bs) t
  let lbs = length bs
  when (lbs /= 1) $ addError
    (pos c)
    [ Error "The label"
    , Error c
    , Error "should have 1"
    , Error "argument, but has been given"
    , Error $ show lbs
    ]

paramsToVEnvCM :: ProgVar -> [ProgVar] -> T.Type -> FreestState ()
paramsToVEnvCM c bs t = do
  let ts = zipProgVarLType bs t
  mapM_ (uncurry addToVEnv) ts
  let lbs = length bs
      lts = numArgs t
  when (lbs /= lts) $ addError
    (pos c)
    [ Error "The constructor"
    , Error c
    , Error "should have"
    , Error lts
    , Error "arguments, but has been given"
    , Error lbs
    ]

zipProgVarLType :: [ProgVar] -> T.Type -> [(ProgVar, T.Type)]
zipProgVarLType []       _                 = []
zipProgVarLType (b : bs) (T.Fun _ _ t1 t2) = (b, t1) : zipProgVarLType bs t2
zipProgVarLType (b : _ ) t                 = [(b, t)]

numArgs :: T.Type -> Int
numArgs (T.Fun _ _ _ t2) = 1 + numArgs t2
numArgs _                = 0

-- Check whether a constructor exists in a type map
synthetiseCons :: ProgVar -> T.TypeMap -> FreestState T.Type
synthetiseCons x tm = case tm Map.!? x of
  Just t  -> return t
  Nothing -> do
    addError
      (pos x)
      [ Error "Data constructor or field name in choice type"
      , Error x
      , Error "not in scope"
      ]
    return $ T.Skip (pos x)

-- The quotient operation. Removes a program variable from the
-- variable environment and gives an error if it is linear
quotient :: K.KindEnv -> ProgVar -> FreestState ()
quotient kEnv x = do
  getFromVEnv x >>= \case
    Just t -> do
      k <- K.synthetise kEnv t
      when (K.isLin k) $ addError
        (pos x)
        [ Error "Program variable"
        , Error x
        , Error "is linear at the end of its scope\n"
        , Error "\t variable"
        , Error x
        , Error "is of type"
        , Error t
        , Error "of kind"
        , Error k
        ]
    Nothing -> return ()
  removeFromVEnv x

addPartiallyAppliedError :: E.Exp -> String -> FreestState T.Type
addPartiallyAppliedError e s = do
  let p = pos e
  addError
    p
    [ Error "Ooops! You're asking too much. I cannot type a partially applied"
    , Error e
    , Error "\b.\n\t I promise to look into that some time in the future.\n"
    , Error "\t In the meantime consider applying"
    , Error e
    , Error $ "to an expression denoting a " ++ s ++ "."
    ]
  return $ omission p

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> FreestState ()
-- Boolean elimination
checkAgainst kEnv (E.Conditional p e1 e2 e3) t = do
  -- let kEnv = kEnvFromType kEnv t
  checkAgainst kEnv e1 (T.Bool p)
  vEnv2 <- getVEnv
  checkAgainst kEnv e2 t
  vEnv3 <- getVEnv
  setVEnv vEnv2
  checkAgainst kEnv e3 t
  vEnv4 <- getVEnv
  checkEquivEnvs p "conditional" kEnv vEnv3 vEnv4
-- Pair elimination
checkAgainst kEnv (E.BinLet _ x y e1 e2) t2 = do
  -- let kEnv = kEnvFromType kEnv t2
  t1       <- synthetise kEnv e1
  (u1, u2) <- extractPair e1 t1
  addToVEnv x u1
  addToVEnv y u2
  checkAgainst kEnv e2 t2
  quotient kEnv x
  quotient kEnv y
-- TODO Match
-- checkAgainst kEnv (Match p e fm) = checkAgainstFieldMap p kEnv e fm extractInChoiceMap
-- TODO Datatype elimination
-- checkAgainst kEnv (Case p e fm) = checkAgainstFieldMap p kEnv e fm extractDatatypeMap
-- Abs elimination. It seems that we cannot do checkAgainst for we
-- cannot decide whether to use a Lin or a Un function. See
-- counterexamples: polySUTL.fst when using Lin, and mult.fst when
-- using Un
-- checkAgainst kEnv (App p e1 e2) u = do
--   t <- synthetise kEnv e2
--   checkAgainst kEnv e1 (Fun p Un/Lin t u)
-- Default
checkAgainst kEnv e t = do
  -- let kEnv = kEnvFromType kEnv t
  u <- synthetise kEnv e
--  traceM $ "checkAgainst exp " ++ show e ++ " - "  ++ show u 
  checkEquivTypes e kEnv t u

-- | Check an expression against a given type scheme
-- checkAgainstTS :: E.Exp -> TypeScheme -> FreestState ()
-- checkAgainstTS e (TypeScheme _ bs t) = checkAgainst (fromKindBinds bs) e t

kEnvFromType :: K.KindEnv -> T.Type -> K.KindEnv
kEnvFromType kenv (T.Forall _ (K.Bind _ x k) t) =
  kEnvFromType (Map.insert x k kenv) t
kEnvFromType kenv _ = kenv


-- EQUALITY AND EQUIVALENCE CHECKING

checkEquivTypes :: E.Exp -> K.KindEnv -> T.Type -> T.Type -> FreestState ()
checkEquivTypes exp kEnv expected actual = do
--  tEnv <- getTEnv
  -- vEnv <- getVEnv
  -- traceM ("\n checkEquivTypes exp : " ++ show exp ++ " \t" ++ show (userDefined vEnv))
  unless (equivalent kEnv actual expected) $ addError
    (pos exp)
    [ Error "Couldn't match expected type"
    , Error expected
    , Error "\n\t             with actual type"
    , Error actual
    , Error "\n\t               for expression"
    , Error exp
    ]

checkEqualEnvs :: E.Exp -> T.VarEnv -> T.VarEnv -> FreestState ()
checkEqualEnvs e vEnv1 vEnv2 = unless
  (Map.null diff)
  (addError
    (pos e)
    [ Error
      "Final environment differs from initial in an unrestricted function\n"
    , Error "\t These extra entries are present in the final environment:"
    , Error diff
    , Error "\n\t for lambda abstraction"
    , Error e
    ]
  )
  where diff = Map.difference vEnv2 vEnv1

checkEquivEnvs
  :: Pos -> String -> K.KindEnv -> T.VarEnv -> T.VarEnv -> FreestState ()
checkEquivEnvs p branching kEnv vEnv1 vEnv2 = do
--  tEnv <- getTEnv
  let vEnv1' = userDefined vEnv1
      vEnv2' = userDefined vEnv2
  unless (equivalent kEnv vEnv1' vEnv2') $ addError
    p
    [ Error
      (  "I have reached the end of a "
      ++ branching
      ++ " expression and found two distinct typing environments."
      )
    , Error "\n\t They are "
    , Error (vEnv1' Map.\\ vEnv2')
    , Error "\n\t      and "
    , Error (vEnv2' Map.\\ vEnv1')
    , Error
      "\n\t (is a given variable consumed in one branch and not in the other?)"
    , Error
      "\n\t (is there a variable with different types in the two environments?)"
    ]

fillFunType :: K.KindEnv -> ProgVar -> E.Exp -> T.Type -> FreestState T.Type
fillFunType kEnv b = fill
 where
  fill :: E.Exp -> T.Type -> FreestState T.Type
  fill (E.Abs _ _ (T.Bind _ b _) e) (T.Fun _ _ t1 t2) = do
    addToVEnv b t1
    t3 <- fill e t2
    removeFromVEnv b
    return t3
  fill e@(E.Abs p _ _ _) t = do
    addError
      (pos b)
      [ Error "Couldn't match expected type"
      , Error t
      , Error "\n\t The equation for"
      , Error b
      , Error "has one or more arguments,"
      , Error "\n\t but its type"
      , Error t
      , Error "has none"
      ]
    return t
  fill e _ = synthetise kEnv e
