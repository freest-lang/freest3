{-# LANGUAGE BangPatterns, LambdaCase #-}
module Interpreter.Eval (evalAndPrint) where

import           Control.Concurrent (forkIO)
import           Control.Monad (liftM, liftM2)
import qualified Data.Map as Map
import           Interpreter.Builtin
import           Interpreter.Value
import           Syntax.Base
import qualified Syntax.Expressions as E
import           Syntax.ProgramVariables

------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

evalAndPrint :: Ctx -> E.ExpEnv -> E.Expression -> IO ()
evalAndPrint ctx eenv e = do
  res <- eval ctx eenv e
  case res of
    IOValue io -> io >>= putStrLn . show
    _          -> putStrLn $ show res

eval :: Ctx -> E.ExpEnv -> E.Expression -> IO Value
eval _ _ (E.Unit _)        = return Unit
eval _ _ (E.Integer _ i)   = return $ Integer i
eval _ _ (E.Boolean _ b)   = return $ Boolean b
eval _ _ (E.Character _ c) = return $ Character c
eval ctx eenv (E.ProgVar _ x) = evalVar ctx eenv x
eval ctx eenv (E.TypeApp _ x _) = evalVar ctx eenv x
eval ctx _ (E.Lambda _ _ x _ e) = return $ Closure x e ctx
eval ctx eenv (E.App _ e1 e2) =
  eval ctx eenv e1 >>= \case
    (Closure x e ctx') -> do
      !v <- eval ctx eenv e2
      eval (Map.insert x v ctx') eenv e
    (PrimitiveFun f) -> do
      !v <- eval ctx eenv e2
      case f v of
         (IOValue res) -> do
           !r <- res
           pure $ r
         r -> return r
    (Cons x xs) -> do
      !v <- eval ctx eenv e2
      pure $ Cons x (xs ++ [[v]])

eval ctx eenv (E.Pair _ e1 e2) =
  liftM2 Pair (eval ctx eenv e1) (eval ctx eenv e2)

eval ctx eenv (E.BinLet _ x y e1 e2) = do
  !(Pair v1 v2) <- eval ctx eenv e1
  let env = Map.insert x v1 (Map.insert y v2 ctx)
  eval env eenv e2

eval ctx eenv (E.Conditional _ cond e1 e2) = do
  (Boolean b) <- eval ctx eenv cond 
  if b then eval ctx eenv e1 else eval ctx eenv e2
    
eval ctx eenv (E.UnLet _ x e1 e2) = do
  !v <- eval ctx eenv e1
  eval (Map.insert x v ctx) eenv e2

eval ctx eenv (E.Case _ e m) =
  eval ctx eenv e >>= evalCase ctx eenv m

eval ctx eenv (E.Fork _ e) = do
  _ <- forkIO $ eval ctx eenv e >> return ()
  return Unit

eval _ _ (E.New _ _ _) = do
  (c1, c2) <- new
  return $ Pair (Chan c1) (Chan c2) 

eval ctx eenv (E.Send _ e) = do
  (Chan c) <- eval ctx eenv e
  return $ PrimitiveFun (\v ->
                IOValue $ liftM Chan (send c v))
  
eval ctx eenv (E.Receive _ e) = do
   (Chan c) <- eval ctx eenv e
   !(v, c) <- receive c
   return $ Pair v (Chan c)

eval ctx eenv (E.Select _ e x) = do
   (Chan c) <- eval ctx eenv e
   !c <- send c (Label (show x))
   return $ Chan c

eval ctx eenv (E.Match _ e m) = do
  (Chan c) <- eval ctx eenv e
  (Label !v, !c) <- receive c 
  let (patterns:_, e) = m Map.! (mkVar defaultPos v)
  let ctx' = Map.insert patterns (Chan c) ctx
  eval ctx' eenv e

evalCase :: Ctx -> E.ExpEnv -> E.FieldMap -> Value -> IO Value
evalCase ctx eenv m (Cons x xs) = do
  let !(patterns, e) = m Map.! x
  let lst  = zip patterns xs
  let ctx1 = foldl (\acc (c,xs) -> Map.insert c (head xs) acc) ctx lst  
  eval ctx1 eenv e

evalVar :: Ctx -> E.ExpEnv -> ProgVar -> IO Value
evalVar ctx eenv x
  | isADT x           = return $ Cons x [] -- TODO: change isADT definition
  | Map.member x eenv = do
--      traceM $ show x
      eval ctx eenv (eenv Map.! x)
  | Map.member x ctx  = return $ ctx Map.! x
  | otherwise         = error $ "error evaluating progvar: " ++ show x
