{-# LANGUAGE BangPatterns, LambdaCase #-}
module Interpreter.Eval
  ( evalAndPrint
  , evaluate
  , evalAndPrint'
  )
where

import           Interpreter.Builtin
import           Interpreter.Value
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName
import           Syntax.Program
import           Util.Error
import           Util.State hiding (void)
import           Typing.Phase
import           Syntax.AST

import           Control.Concurrent ( forkIO )
import           Data.Functor
import qualified Data.Map as Map
import           System.Exit ( die )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.IO ( hFlush, stdout, stderr )
import Control.Monad (foldM)
import Syntax.Value (isVal)
-- import Debug.Trace (trace)

------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

evalAndPrint :: Variable -> TypingS -> IO ()
evalAndPrint m s = do
  ctx' <-  foldM evalDefs initialCtx (evalOrder $ ast s)
  hFlush stdout
  hFlush stderr
  case ctx' Map.!? m of
    Just(IOValue io) -> io >>= print
    Just res         -> print res
    Nothing          -> return ()
  where
    evalDefs :: Ctx -> [Variable] -> IO Ctx 
    evalDefs ctx xs = 
      let eenv = getDefsS s in 
      let recEnv = Map.fromList $ map (\x -> (x,eenv Map.! x)) xs in 
      foldM (evalDef recEnv) ctx xs
    evalDef :: Defs -> Ctx -> Variable -> IO Ctx
    evalDef recEnv ctx x = do
      case getDefsS s Map.!? x of 
        Just e -> do !v' <- eval x (getTypesS s) ctx recEnv e
                     return $ Map.insert x v' ctx
        Nothing -> return ctx 

evaluate :: Ctx -> TypingS -> IO Ctx 
evaluate ctx s = foldM evalDefs ctx (evalOrder $ ast s)
  where
    evalDefs :: Ctx -> [Variable] -> IO Ctx 
    evalDefs ctx xs = 
      let eenv = getDefsS s in 
      let recEnv = Map.fromList $ map (\x -> (x,eenv Map.! x)) xs in 
      foldM (evalDef recEnv) ctx xs
    evalDef :: Defs -> Ctx -> Variable -> IO Ctx
    evalDef recEnv ctx x = do
      case getDefsS s Map.!? x of 
        Just e -> do !v' <- eval x (getTypesS s) ctx recEnv e
                     return $ Map.insert x v' ctx
        Nothing -> return ctx 

evalAndPrint' :: Variable -> Ctx -> TypingS -> IO (Value, Ctx) 
evalAndPrint' x ctx s = do 
  --putStrLn ("###############\nctx: "++show ctx++"\n############\nevalOrder: "++show (evalOrder (ast s)))
  ctx' <- evaluate ctx s 
  --putStrLn ("###############\nctx: "++show ctx')
  hFlush stdout
  hFlush stderr
  case ctx' Map.! x of
    res@(IOValue io) -> io >>= print >> return (res,ctx')
    res              -> print res >> return (res,ctx')

-- evalAndPrintVals :: Variable -> TypingS -> IO ()
-- evalAndPrintVals m s = do
--   let xs = concat $ evalOrder $ ast s
--   let ys = sortBy (compare `on` getSpan) $ Map.keys $ getDefsS s
--   putStrLn ("xs="++show xs)
--   putStrLn ("ys="++show ys)
--   let xds = filter (\x -> Map.member x (getDefsS s)) xs 
--   let valNames = filter (isVal . (getDefsS s Map.!)) xds
--   let nonValNames = filter (not . isVal . (getDefsS s Map.!)) xds
--   -- putStrLn ("valNames = "++show valNames)
--   -- putStrLn ("nonValNames = "++show nonValNames)
--   let valenv = (Map.filter isVal (getDefsS s))
--   ctx' <-  foldM (evalDef valenv) initialCtx  xds
--   hFlush stdout
--   hFlush stderr
--   case ctx' Map.!? m of
--     Just(IOValue io) -> io >>= print
--     Just res         -> print res
--     Nothing          -> return ()
--   where
--     evalDef :: Defs -> Ctx -> Variable -> IO Ctx
--     evalDef valenv ctx x = do
--       let eenv = getDefsS s
--           t    = eenv Map.!? x
--       case t of 
--         Just u -> do !v' <- eval x (getTypesS s) ctx (if isVal u then valenv else Map.empty) u
--                      return $ Map.insert x v' ctx
--         Nothing -> return ctx -- putStrLn ("'"++show x++"' is a builtin ("++show (Map.member x ctx)++"). Nothing to evaluate.")


-- evalAndPrint' :: Variable -> TypingS -> E.Exp -> IO ()
-- evalAndPrint' name s e = do
--   ctx <- addPrimitiveChannels ["stdout", "stdin", "stderr"] initialCtx
--   res <- eval name (getTypesS s) ctx e
--   hFlush stdout
--   hFlush stderr
--   case res of
--     IOValue io -> io >>= print
--     _          -> print res
--   where
--     addPrimitiveChannels :: [String] -> Ctx -> IO Ctx
--     addPrimitiveChannels [] ctx = return ctx
--     addPrimitiveChannels (varName : varNames) ctx = do
--       (clientChan, serverChan) <- new
--       addPrimitiveChannels varNames
--         $ Map.insert (mkVar defaultSpan         varName  ) (Chan clientChan)
--         $ Map.insert (mkVar defaultSpan ("__" ++ varName)) (Chan serverChan) ctx


eval :: Variable -> Types -> Ctx -> Defs -> E.Exp -> IO Value
eval _ _ _   _ (E.Unit _                  ) = return Unit
eval _ _ _   _ (E.Int    _ i              ) = return $ Integer i
eval _ _ _   _ (E.Float  _ f              ) = return $ Float f        
eval _ _ _   _ (E.Char   _ c              ) = return $ Character c
eval _ _ _   _ (E.String _ s              ) = return $ String s
eval _ _ ctx eenv (E.TypeAbs _ (Bind _ _ _ e)) = return $ TypeAbs e ctx eenv 
eval fun _ ctx eenv (E.Abs _ _ (Bind _ x _ e)) = return $ Closure fun x e ctx eenv 
eval fun tys ctx eenv (E.Var    _ x       ) = evalVar fun tys ctx eenv x
eval fun tys ctx eenv (E.TypeApp _ e _    ) = eval fun tys ctx eenv e >>= \case
  (TypeAbs v ctx eenv) -> eval fun tys ctx eenv v
  v -> return v
eval fun tys ctx eenv (E.App p (E.Var _ x) e)
  | x == mkSelect p =
      return $ PrimitiveFun (\(Chan c) -> IOValue $ Chan <$> send (Label $ show e) c)
  | x == mkCollect p = eval fun tys ctx eenv e
eval fun tys ctx eenv (E.App _ e1 e2) = eval fun tys ctx eenv e1 >>= \case
  (Closure fun x e ctx' eenv') -> do
    !v <- eval fun tys ctx eenv e2
    eval fun tys (Map.insert x v ctx') eenv' e
  Fork -> forkIO (void $ eval fun tys ctx eenv (E.App (getSpan e2) e2 (E.Unit (getSpan e2)))) $> Unit
  (PrimitiveFun f) -> do
    !v <- eval fun tys ctx eenv e2
    case f v of
      (IOValue res) -> do
        !r <- res
        pure r
      r -> pure r
  (Cons x xs) -> do
    !v <- eval fun tys ctx eenv e2
    pure $ Cons x (xs ++ [[v]])
  c -> pure c
eval fun tys ctx eenv (E.Pair _ e1 e2)  = Pair <$> eval fun tys ctx eenv e1 <*> eval fun tys ctx eenv e2
eval fun tys ctx eenv (E.BinLet _ x y e1 e2) = do
  (Pair v1 v2) <- eval fun tys ctx eenv e1
  eval fun tys (Map.insert x v1 (Map.insert y v2 ctx)) eenv e2
eval fun tys ctx eenv (E.UnLet _ x e1 e2) = do
  !v <- eval fun tys ctx eenv e1
  eval fun tys (Map.insert x v ctx) eenv e2
eval fun tys ctx eenv (E.Case s e m) = eval fun tys ctx eenv e >>=  evalCase fun s tys ctx eenv m 
-- Pack & Unpack
eval fun tys ctx eenv (E.Pack _ _ e _)  = eval fun tys ctx eenv e
eval fun tys ctx eenv (E.Unpack _ _ x e1 e2)  = do
  !v <- eval fun tys ctx eenv e1
  eval fun tys (Map.insert x v ctx) eenv e2  
eval fun _ _ _ e = internalError "Interpreter.Eval.eval" e


evalCase :: Variable -> Span -> Types -> Ctx -> Definitions Typing -> E.FieldMap -> Value -> IO Value
evalCase name _ tys ctx eenv m (Chan c0) = do
  (Label !v, !c) <- receive c0
  let (patterns : _, e) = m Map.! mkVar defaultSpan v
  let ctx'              = Map.insert patterns (Chan c) ctx
  eval name tys ctx' eenv e
evalCase name s tys ctx eenv m (Cons x xs) =
  case m Map.!? x of
    Nothing ->
      let msg = "Non-exhaustive patterns in function " ++ show name in
      die $ showError True (Right "") Map.empty (RuntimeError s msg)
    Just (patterns, e) -> 
      let lst            = zip patterns xs in
      let ctx1 = foldl (\acc (c, y : _) -> Map.insert c y acc) ctx lst in 
      eval name tys ctx1 eenv e 
evalCase _ _ _ _ _ _ v = internalError "Interpreter.Eval.evalCase" v

evalVar :: Variable -> Types -> Ctx -> Defs -> Variable -> IO Value
evalVar name tEnv ctx eenv x
  | isDatatypeContructor x tEnv = return $ Cons x []
  | Map.member x eenv           = eval x tEnv ctx eenv (eenv Map.! x)
  | Map.member x ctx            = return $ ctx Map.! x
  | x == mkFork defaultSpan     = return Fork
  | x == mkError                =
     return $ PrimitiveFun (\(String e) -> exception (ErrorFunction (getSpan x) e))
  | x == mkUndefined            =
     return $ exception (UndefinedFunction (getSpan x))
  | otherwise                   = internalError ("Interpreter.Eval.evalVar: could not find "++show x++" while evaluating the definition of "++show name++". \n ctx="++show (Map.keysSet ctx)++"\n eenv="++show (Map.keysSet eenv)) x
  where
    exception err = unsafePerformIO $ die $ showError False (Right "") Map.empty err
