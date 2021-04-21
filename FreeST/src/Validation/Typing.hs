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

{-# LANGUAGE LambdaCase, TupleSections, MultiWayIf #-}

module Validation.Typing
  ( synthetise
  , checkAgainst
--  , fillFunType
  )
where

import           Control.Monad.State            ( when
                                                , unless
                                                , void
                                                )
import qualified Data.Map.Strict               as Map
import           Equivalence.Equivalence
import           Parse.Unparser -- debug
import           Syntax.Base
import qualified Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Util.FreestState
import           Util.PreludeLoader             ( userDefined ) -- debug
import qualified Validation.Extract            as Extract
import qualified Validation.Kinding            as K -- Again?
import qualified Validation.Rename             as Rename
                                                ( subs )

import           Data.Functor

-- SYNTHESISING A TYPE

synthetise :: K.KindEnv -> E.Exp -> FreestState T.Type
-- Basic expressions
synthetise _    (E.Int  p _       ) = return $ T.Int p
synthetise _    (E.Char p _       ) = return $ T.Char p
synthetise _    (E.Bool p _       ) = return $ T.Bool p
synthetise _    (E.Unit p         ) = return $ T.Unit p
synthetise _    (E.String p _     ) = return $ T.String p
-- Variable
synthetise kEnv (E.Var    p x     ) = synthetiseVar kEnv x
-- synthetise kEnv e@(E.Var p x)
--   | x == mkVar p "receive" = addPartiallyAppliedError e "channel"
--   | x == mkVar p "send" = addPartiallyAppliedError
--     e
--     "value and another denoting a channel"
--   | x == mkVar p "branch" = addPartiallyAppliedError e "channel"
--   | otherwise = synthetiseVar kEnv x
synthetise kEnv (E.UnLet _ x e1 e2) = do
  t1 <- synthetise kEnv e1
  addToVEnv x t1
  t2 <- synthetise kEnv e2
  quotient kEnv x
  return t2
-- Abs introduction
synthetise kEnv e'@(E.Abs p (E.Bind _ m x t1 e)) = do
  void $ K.synthetise kEnv t1
  vEnv1 <- getVEnv
  addToVEnv x t1
  t2 <- synthetise kEnv e
  quotient kEnv x
  vEnv2 <- getVEnv
--  checkEqualEnvs e' vEnv1 vEnv2
  when (m == Un) (checkEqualEnvs e' vEnv1 vEnv2)
  return $ T.Fun p m t1 t2
-- Abs elimination
synthetise kEnv (E.App p (E.Select _ c) e) = do
  t <- synthetise kEnv e
  m <- Extract.outChoiceMap e t
  Extract.constructor p m c
  -- Fork e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkVar p "fork" = do
  t <- synthetise kEnv e
  void $ K.checkAgainst kEnv (K.tu p) t
  return $ T.Unit p
  -- Receive e
synthetise kEnv (E.App p (E.Var _ x) e) | x == mkVar p "receive" = do
  t        <- synthetise kEnv e
  (u1, u2) <- Extract.input e t
  void $ K.checkAgainst kEnv (K.ml (pos u1)) u1
  return $ T.Pair p u1 u2
  -- collect e
synthetise kEnv (E.App _ (E.Var p x) e) | x == mkVar p "collect" = do
  tm <- Extract.inChoiceMap e =<< synthetise kEnv e
  return $ T.Datatype p $ Map.map (\u -> T.Fun p Un u (T.Unit defaultPos)) tm
-- synthetise kEnv (E.App p (E.TypeApp _ (E.Var _ x) t) e)
--   | x == mkVar p "branch" = do
--     tm <- Extract.inChoiceMap e =<< synthetise kEnv e
--     return $ T.Datatype p $ Map.map (\u -> T.Fun p Un u t) tm
  -- Send e
-- synthetise _ e@(E.App p (E.Var _ x) _) | x == mkVar p "send" =
--   addPartiallyAppliedError e "channel"
  -- Send e1 e2
synthetise kEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkVar p "send" = do
  t        <- synthetise kEnv e2
  (u1, u2) <- Extract.output e2 t
  void $ K.checkAgainst kEnv (K.ml (pos u1)) u1
  checkAgainst kEnv e1 u1
  return u2
synthetise kEnv (E.App _ e1 e2) = do -- General case
  t        <- synthetise kEnv e1
  (u1, u2) <- Extract.function e1 t
  checkAgainst kEnv e2 u1
  return u2
-- Type Abstraction intro and elim
synthetise kEnv (E.TypeAbs _ (K.Bind p a k e)) = -- do
--  t <- synthetise (Map.insert a k kEnv) e
  T.Forall p . K.Bind p a k <$> synthetise (Map.insert a k kEnv) e
-- Type application
synthetise kEnv (E.TypeApp _ e t) = do
  u                               <- synthetise kEnv e
  ~(T.Forall _ (K.Bind _ y k u')) <- Extract.forall e u
  void $ K.checkAgainst kEnv k t
  return $ Rename.subs t y u'
-- Boolean elimination
synthetise kEnv (E.Cond p e1 e2 e3) = do
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
  return $ T.Pair p t1 t2
-- Pair elimination
synthetise kEnv (E.BinLet _ x y e1 e2) = do
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToVEnv x u1
  addToVEnv y u2
  t2 <- synthetise kEnv e2
  quotient kEnv x
  quotient kEnv y
  return t2
-- Datatype elimination
synthetise kEnv (E.Case p e fm) =
  synthetiseCase p kEnv e fm Extract.datatypeMap
-- Session types
synthetise kEnv (E.New p t u) = do
  K.checkAgainstSession kEnv t
  return $ T.Pair p t u
synthetise _ e@(E.Select _ _) = addPartiallyAppliedError e "channel of a + type"

-- | Returns the type of a variable; removes it from vEnv if lin
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
    -- , Error "\b.\n\t I promise to look into that some time in the future.\n"
    -- , Error "\t In the meantime consider applying"
    , Error "\n\t Consider applying"
    , Error e
    , Error $ "to an expression denoting a " ++ s ++ "."
    ]
  return $ omission p

-- CHECKING AGAINST A GIVEN TYPE

-- | Check an expression against a given type
checkAgainst :: K.KindEnv -> E.Exp -> T.Type -> FreestState ()
-- Boolean elimination
checkAgainst kEnv (E.Cond p e1 e2 e3) t = do
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
  t1       <- synthetise kEnv e1
  (u1, u2) <- Extract.pair e1 t1
  addToVEnv x u1
  addToVEnv y u2
  checkAgainst kEnv e2 t2
  quotient kEnv x
  quotient kEnv y
-- TODO Match
-- checkAgainst kEnv (Match p e fm) = checkAgainstFieldMap p kEnv e fm Extract.inChoiceMap
-- TODO Datatype elimination
-- checkAgainst kEnv (Case p e fm) = checkAgainstFieldMap p kEnv e fm Extract.datatypeMap
-- Abs elimination. It seems that we cannot do checkAgainst for we
-- cannot decide whether to use a Lin or a Un function. See
-- counterexamples: polySUTL.fst when using Lin, and mult.fst when
-- using Un
-- checkAgainst kEnv (App p e1 e2) u = do
--   t <- synthetise kEnv e2
--   checkAgainst kEnv e1 (Fun p Un/Lin t u)
checkAgainst kEnv e t = checkEquivTypes e kEnv t =<< synthetise kEnv e

-- EQUALITY AND EQUIVALENCE CHECKING

checkEquivTypes :: E.Exp -> K.KindEnv -> T.Type -> T.Type -> FreestState ()
checkEquivTypes exp kEnv expected actual =
  unless (equivalent kEnv actual expected) $ addError
    (pos exp)
    [ Error "Couldn't match expected type"
    , Error expected
    , Error "\n\t             with actual type"
    , Error actual
    , Error "\n\t               for expression"
    , Error exp
    ]

checkEqualEnvs :: E.Exp -> VarEnv -> VarEnv -> FreestState ()
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
  :: Pos -> String -> K.KindEnv -> VarEnv -> VarEnv -> FreestState ()
checkEquivEnvs p branching kEnv vEnv1 vEnv2 = do
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

-- TODO: later, turn into warning
-- TODO: remove bool and string (see comment below)
buildMap :: Pos -> T.TypeMap -> E.FieldMap -> FreestState E.FieldMap
buildMap p tm fm
  | Map.size tm /= Map.size fm
  = addError
      p
      [ Error "Wrong number of constructors\n"
      , Error "\t The expression has"
      , Error $ Map.size fm
      , Error "constructor(s)\n"
      , Error "\t but the type has"
      , Error $ Map.size tm
      , Error "constructor(s)\n"
      , Error $ "\t in case "
      , Error $ "\ESC[91m{" ++ showFieldMap fm ++ "}\ESC[0m"
      ]
    >> pure fm
  | otherwise
  = tMapWithKeyM (buildAbstraction tm) fm


buildAbstraction
  :: T.TypeMap
  -> ProgVar
  -> ([ProgVar], E.Exp)
  -> FreestState ([ProgVar], E.Exp)
buildAbstraction tm x (xs, e) = case tm Map.!? x of
  Just t -> if numberOfArgs t /= length xs
    then diffArgsErr (numberOfArgs t) $> (xs, e)
    else (xs, ) <$> buildAbstraction' (xs, e) t
  Nothing ->
    addError (pos x) [Error "Data constructor", Error x, Error "not in scope"]
      $> (xs, e)
 where
  buildAbstraction' :: ([ProgVar], E.Exp) -> T.Type -> FreestState E.Exp
  buildAbstraction' ([], e) _ = pure e
  buildAbstraction' (x : xs, e) (T.Fun _ _ t1 t2) = -- m ?? Un ??
    E.Abs (pos e) . E.Bind (pos e) Un x t1 <$> buildAbstraction' (xs, e) t2
  buildAbstraction' ([x], e) t =
    return $ E.Abs (pos e) $ E.Bind (pos e) Un x t e

  diffArgsErr :: Int -> FreestState ()
  diffArgsErr i = addError
    (pos e)
    [ Error "The constructor"
    , Error x
    , Error "should have"
    , Error i
    , Error "arguments, but has been given"
    , Error $ length xs
    , Error "\n\t In the pattern:"
    , Error $ show x ++ " " ++ unwords (map show xs) ++ " -> " ++ show e
    ]

  numberOfArgs :: T.Type -> Int
  numberOfArgs (T.Fun _ _ _ t) = 1 + numberOfArgs t
  numberOfArgs _               = 0

synthetiseCase
  :: Pos
  -> K.KindEnv
  -> E.Exp
  -> E.FieldMap
  -> (E.Exp -> T.Type -> FreestState T.TypeMap)
  -> FreestState T.Type
synthetiseCase p kEnv e fm extract = do
  tm                <- extract e =<< synthetise kEnv e
  newMap            <- buildMap p tm fm
  vEnv              <- getVEnv
  ~(t : ts, v : vs) <- Map.foldr (synthetiseMap kEnv vEnv)
                                 (return ([], []))
                                 newMap
  mapM_ (checkEquivTypes e kEnv t)       ts
  mapM_ (checkEquivEnvs p "case" kEnv v) vs
  setVEnv v
  return t


synthetiseMap
  :: K.KindEnv
  -> VarEnv
  -> ([ProgVar], E.Exp)
  -> FreestState ([T.Type], [VarEnv])
  -> FreestState ([T.Type], [VarEnv])
synthetiseMap kEnv vEnv (x, e) state = do
  (ts, envs) <- state
  t          <- synthetise kEnv e
  env        <- getVEnv
  setVEnv vEnv
  return (returnType (length x) t : ts, env : envs)
 where
  returnType :: Int -> T.Type -> T.Type
  returnType 0 t                = t
  returnType i (T.Fun _ _ _ t2) = returnType (i - 1) t2
  returnType _ t                = t
