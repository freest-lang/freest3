{-# LANGUAGE BangPatterns, LambdaCase #-}
module Interpreter.Eval
  ( evalAndPrint
  )
where


import           Interpreter.Builtin
import           Interpreter.Value
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.Program
import           Util.Error

import           Control.Concurrent ( forkIO )
import           Data.Functor
import qualified Data.Map as Map
import           Prelude hiding (span)
import           System.Exit ( die )
import           System.IO.Unsafe ( unsafePerformIO )

------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

evalAndPrint :: TypeEnv -> Ctx -> Prog -> E.Exp -> IO ()
evalAndPrint tEnv ctx eenv e = do
  res <- eval tEnv ctx eenv e
  case res of
    IOValue io -> io >>= print
    _          -> print res

eval :: TypeEnv -> Ctx -> Prog -> E.Exp -> IO Value
eval _ _   _ (E.Unit _                      )    = return Unit
eval _ _   _ (E.Int    _ i                  )    = return $ Integer i
eval _ _   _ (E.Bool   _ b                  )    = return $ Boolean b
eval _ _   _ (E.Char   _ c                  )    = return $ Character c
eval _ _   _ (E.String _ s                  )    = return $ String s
eval _ ctx _ (E.TypeAbs _ (Bind _ _ _ e))        = return $ TypeAbs e ctx
eval _ ctx _ (E.Abs _ _ (Bind _ x _ e))          = return $ Closure x e ctx
eval tEnv ctx eenv (E.Var    _ x            )    = evalVar tEnv ctx eenv x
eval tEnv ctx eenv (E.TypeApp _ e _         )    = eval tEnv ctx eenv e >>= \case
  (TypeAbs v ctx) -> eval tEnv ctx eenv v
  v -> return v
eval tEnv ctx eenv (E.App p (E.Var _ x) e)
  | x == mkVar p "select" =
      return $ PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send (Label $ show e) c)
  | x == mkVar p "collect" = eval tEnv ctx eenv e
eval tEnv ctx eenv (E.App _ e1 e2) = eval tEnv ctx eenv e1 >>= \case
  (Closure x e ctx') -> do
    !v <- eval tEnv ctx eenv e2
    eval tEnv (Map.insert x v ctx') eenv e
  Fork -> forkIO (void $ eval tEnv ctx eenv e2) $> Unit
  (PrimitiveFun f) -> do
    !v <- eval tEnv ctx eenv e2
    case f v of
      (IOValue res) -> do
        !r <- res
        pure r
      r -> pure r
  (Cons x xs) -> do
    !v <- eval tEnv ctx eenv e2
    pure $ Cons x (xs ++ [[v]])
  e -> error $ show e
eval tEnv ctx eenv (E.Pair _ e1 e2)  = Pair <$> eval tEnv ctx eenv e1 <*> eval tEnv ctx eenv e2
eval tEnv ctx eenv (E.BinLet _ x y e1 e2) = do
  (Pair v1 v2) <- eval tEnv ctx eenv e1
  eval tEnv (Map.insert x v1 (Map.insert y v2 ctx)) eenv e2
eval tEnv ctx eenv (E.Cond _ cond e1 e2) = do
  (Boolean b) <- eval tEnv ctx eenv cond 
  if b then eval tEnv ctx eenv e1 else eval tEnv ctx eenv e2
eval tEnv ctx eenv (E.UnLet _ x e1 e2) = do
  !v <- eval tEnv ctx eenv e1
  eval tEnv (Map.insert x v ctx) eenv e2
eval tEnv ctx eenv (E.Case _ e m)  = eval tEnv ctx eenv e >>= evalCase tEnv ctx eenv m
eval _ _   _    E.New{}        = do
  (c1, c2) <- new
  return $ Pair (Chan c1) (Chan c2)

evalCase :: TypeEnv -> Ctx -> Prog -> E.FieldMap -> Value -> IO Value
evalCase tEnv ctx eenv m (Chan c) = do
  (Label !v, !c) <- receive c
  let (patterns : _, e) = m Map.! mkVar defaultSpan v
  let ctx'              = Map.insert patterns (Chan c) ctx
  eval tEnv ctx' eenv e
evalCase tEnv ctx eenv m (Cons x xs) = do
  let !(patterns, e) = m Map.! x
  let lst            = zip patterns xs
  let ctx1 = foldl (\acc (c, y : _) -> Map.insert c y acc) ctx lst
  eval tEnv ctx1 eenv e 
evalCase _ _ _ _ v = internalError "Interpreter.Eval.evalCase" v

evalVar :: TypeEnv -> Ctx -> Prog -> Variable -> IO Value
evalVar tEnv ctx eenv x
  | isDatatypeContructor x tEnv    = return $ Cons x []
  | Map.member x eenv              = eval tEnv ctx eenv (eenv Map.! x)
  | Map.member x ctx               = return $ ctx Map.! x
  | x == mkVar defaultSpan "fork"  = return Fork
  | x == mkVar defaultSpan "error" = return $ PrimitiveFun
      (\(String e) -> unsafePerformIO $ die $
          showErrors False "" Map.empty (ErrorFunction (span x) e))
  | otherwise                      = internalError "Interpreter.Eval.evalVar" x
