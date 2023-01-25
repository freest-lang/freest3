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
import           Syntax.MkName
import           Util.Error
import           Util.FreestState

import           Control.Concurrent ( forkIO )
import           Data.Functor
import qualified Data.Map as Map
import           System.Exit ( die )
import           System.IO.Unsafe ( unsafePerformIO )
import Debug.Trace (trace)
------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

evalAndPrint :: Variable -> FreestS -> E.Exp -> IO ()
evalAndPrint name s e = 
  addPrimitiveChannels ["stdout", "stdin", "stderr"] initialCtx >>= \ctx -> do

  res <- eval name (typeEnv s) ctx (prog s) e
  case res of
    IOValue io -> io >>= print
    _          -> print res
  
  where
    addPrimitiveChannels :: [String] -> Ctx -> IO Ctx
    addPrimitiveChannels [] ctx = return ctx
    addPrimitiveChannels (varName : varNames) ctx = do
      (clientChan, serverChan) <- new
      addPrimitiveChannels varNames 
        $ Map.insert (mkVar defaultSpan         varName ) (Chan clientChan) 
        $ Map.insert (mkVar defaultSpan ("__" ++ varName)) (Chan serverChan) ctx


eval :: Variable -> TypeEnv -> Ctx -> Prog -> E.Exp -> IO Value
eval _ _ _   _ (E.Unit _                      )    = return Unit
eval _ _ _   _ (E.Int    _ i                  )    = return $ Integer i
eval _ _ _   _ (E.Char   _ c                  )    = return $ Character c
eval _ _ _   _ (E.String _ s                  )    = return $ String s
eval _ _ ctx _ (E.TypeAbs _ (Bind _ _ _ e))        = return $ TypeAbs e ctx
eval fun _ ctx _ (E.Abs _ _ (Bind _ x _ e))          = return $ Closure fun x e ctx
eval fun tEnv ctx eenv (E.Var    _ x            )    = evalVar fun tEnv ctx eenv x
eval fun tEnv ctx eenv (E.TypeApp _ e _         )    = eval fun tEnv ctx eenv e >>= \case
  (TypeAbs v ctx) -> eval fun tEnv ctx eenv v
  v -> return v
eval fun tEnv ctx eenv (E.App p (E.Var _ x) e)
  | x == mkSelect p =
      return $ PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send (Label $ show e) c)
  | x == mkCollect p = eval fun tEnv ctx eenv e
eval fun tEnv ctx eenv (E.App _ e1 e2) = eval fun tEnv ctx eenv e1 >>= \case
  (Closure fun x e ctx') -> do
    !v <- eval fun tEnv ctx eenv e2
    eval fun tEnv (Map.insert x v ctx') eenv e
  Fork -> forkIO (void $ eval fun tEnv ctx eenv (E.App (getSpan e2) e2 (E.Unit (getSpan e2)))) $> Unit
  (PrimitiveFun f) -> do
    !v <- eval fun tEnv ctx eenv e2
    case f v of
      (IOValue res) -> do
        !r <- res
        pure r
      r -> pure r
  (Cons x xs) -> do
    !v <- eval fun tEnv ctx eenv e2
    pure $ Cons x (xs ++ [[v]])
  c -> pure c
eval fun tEnv ctx eenv (E.Pair _ e1 e2)  = Pair <$> eval fun tEnv ctx eenv e1 <*> eval fun tEnv ctx eenv e2
eval fun tEnv ctx eenv (E.BinLet _ x y e1 e2) = do
  (Pair v1 v2) <- eval fun tEnv ctx eenv e1
  eval fun tEnv (Map.insert x v1 (Map.insert y v2 ctx)) eenv e2
eval fun tEnv ctx eenv (E.UnLet _ x e1 e2) = do
  !v <- eval fun tEnv ctx eenv e1
  eval fun tEnv (Map.insert x v ctx) eenv e2
eval fun tEnv ctx eenv (E.Case s e m) = eval fun tEnv ctx eenv e >>=  evalCase fun s tEnv ctx eenv m 

evalCase :: Variable -> Span -> TypeEnv -> Ctx -> Prog -> E.FieldMap -> Value -> IO Value
evalCase name _ tEnv ctx eenv m (Chan c) = do
  (Label !v, !c) <- receive c
  let (patterns : _, e) = m Map.! mkVar defaultSpan v
  let ctx'              = Map.insert patterns (Chan c) ctx
  eval name tEnv ctx' eenv e
evalCase name s tEnv ctx eenv m (Cons x xs) =
  case m Map.!? x of
    Nothing ->
      let msg = "Non-exhaustive patterns in function " ++ show name in
      die $ showErrors True "" Map.empty (RuntimeError s msg)
    Just (patterns, e) -> 
      let lst            = zip patterns xs in
      let ctx1 = foldl (\acc (c, y : _) -> Map.insert c y acc) ctx lst in 
      eval name tEnv ctx1 eenv e 
evalCase _ _ _ _ _ _ v = internalError "Interpreter.Eval.evalCase" v

evalVar :: Variable -> TypeEnv -> Ctx -> Prog -> Variable -> IO Value
evalVar _ tEnv ctx eenv x
  | isDatatypeContructor x tEnv  = return $ Cons x []
  | Map.member x eenv            = eval x tEnv ctx eenv (eenv Map.! x)
  | Map.member x ctx             = return $ ctx Map.! x
  | x == mkFork defaultSpan      = return Fork
  | x == mkError                 =
     return $ PrimitiveFun (\(String e) -> exception (ErrorFunction (getSpan x) e))
  | x == mkUndefined             =
     return $ exception (UndefinedFunction (getSpan x))
  | otherwise                      = internalError "Interpreter.Eval.evalVar" x
  where
    exception err = unsafePerformIO $ die $ showErrors False "" Map.empty err
