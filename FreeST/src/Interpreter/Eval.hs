{-# LANGUAGE BangPatterns, LambdaCase #-}
module Interpreter.Eval
  ( evalAndPrint
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( fmap
                                                , liftM2
                                                , void
                                                )
import qualified Data.Map                      as Map
import           Interpreter.Builtin
import           Interpreter.Value
import           Syntax.Base
import qualified Syntax.Expression             as E
import           Syntax.Program
import           Syntax.ProgramVariable
-- import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import Util.Error(internalError)

------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

evalAndPrint :: Ctx -> Prog -> E.Exp -> IO ()
evalAndPrint ctx eenv e = do
  res <- eval ctx eenv e
  case res of
    IOValue io -> io >>= print
    _          -> print res

eval :: Ctx -> Prog -> E.Exp -> IO Value
eval _   _    (E.Unit _                      ) = return Unit
eval _   _    (E.Int    _ i                  ) = return $ Integer i
eval _   _    (E.Bool   _ b                  ) = return $ Boolean b
eval _   _    (E.Char   _ c                  ) = return $ Character c
eval _   _    (E.String _ s                  ) = return $ String s
eval ctx eenv (E.Var    _ x                  ) = evalVar ctx eenv x
eval ctx eenv (E.TypeApp _ x _               ) = eval ctx eenv x
eval ctx eenv (E.TypeAbs _ (K.Bind _ _ _ e  )) = eval ctx eenv e
eval ctx _    (E.Abs     _ (E.Bind _ _ x _ e)) = return $ Closure x e ctx
eval ctx eenv (E.App p (E.Var _ x) e)
  | x == mkVar p "branch" =
      eval ctx eenv e                                                        
eval ctx eenv (E.App p (E.TypeApp _ (E.Var _ x) t) e)
  | x == mkVar p "branch" =
      eval ctx eenv e                                                        
eval ctx eenv (E.App _ e1 e2                 ) = eval ctx eenv e1 >>= \case
  (Closure x e ctx') -> do
    !v <- eval ctx eenv e2
    eval (Map.insert x v ctx') eenv e
  Fork -> do
    _ <- forkIO (void $ eval ctx eenv e2)
    return Unit
  (PrimitiveFun f) -> do
    !v <- eval ctx eenv e2
    case f v of
      (IOValue res) -> do
        !r <- res
        pure r
      r -> pure r
  (Cons x xs) -> do
    !v <- eval ctx eenv e2
    pure $ Cons x (xs ++ [[v]])
    
  e -> error $ show e
eval ctx eenv (E.Pair _ e1 e2) =
  liftM2 Pair (eval ctx eenv e1) (eval ctx eenv e2)

eval ctx eenv (E.BinLet _ x y e1 e2) = do
  (Pair v1 v2) <- eval ctx eenv e1
  let env = Map.insert x v1 (Map.insert y v2 ctx)
  eval env eenv e2

eval ctx eenv (E.Cond _ cond e1 e2) = do
  (Boolean b) <- eval ctx eenv cond
  if b then eval ctx eenv e1 else eval ctx eenv e2

eval ctx eenv (E.UnLet _ x e1 e2) = do
  !v <- eval ctx eenv e1
  eval (Map.insert x v ctx) eenv e2

eval ctx eenv (E.Case _ e m) = eval ctx eenv e >>= evalCase ctx eenv m

eval _   _    E.New{}        = do
  (c1, c2) <- new
  return $ Pair (Chan c1) (Chan c2)

eval _ _ (E.Select _ x) = return
  $ PrimitiveFun (\(Chan c) -> IOValue $ fmap Chan (send (Label (show x)) c))
  
evalCase :: Ctx -> Prog -> E.FieldMap -> Value -> IO Value
evalCase ctx eenv m c@Chan{} = evalMatch ctx eenv m c
evalCase ctx eenv m (Cons x xs) = do
  let !(patterns, e) = m Map.! x
  let lst            = zip patterns xs
  let ctx1 = foldl (\acc (c, y:_) -> Map.insert c y acc) ctx lst
  eval ctx1 eenv e
evalCase _ _ _ v = internalError "Interpreter.Eval.evalCase" v

evalMatch ctx eenv m (Chan c) = do
  (Label !v, !c) <- receive c
  let (patterns : _, e) = m Map.! mkVar defaultPos v
  let ctx'              = Map.insert patterns (Chan c) ctx
  eval ctx' eenv e


-- TODO: change isADT definition
evalVar :: Ctx -> Prog -> ProgVar -> IO Value
evalVar ctx eenv x | isADT x                      = return $ Cons x []
                   | Map.member x eenv            = eval ctx eenv (eenv Map.! x)
                   | Map.member x ctx             = return $ ctx Map.! x
                   | x == mkVar defaultPos "fork" = return Fork
                   | otherwise = internalError "Interpreter.Eval.evalVar" x
                  -- where
                  --   flip' :: Value
                  --   flip' = Integer 0 -- PrimitiveFun flip 
