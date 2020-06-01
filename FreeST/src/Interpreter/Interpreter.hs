{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# LANGUAGE NoMonadFailDesugaring, LambdaCase #-}
module Interpreter.Interpreter where

import           Control.Monad.State
import Syntax.Expressions
import Syntax.ProgramVariables
import Syntax.Base
import Syntax.Types
import Syntax.Show
import Syntax.Schemes
import qualified Data.Map.Strict as Map
import Data.List -- Debug
import Validation.Rename -- Debug
import Data.Char (isUpper)
import Parse.Parser
import Utils.FreestState
import Debug.Trace
import Text.Show.Functions
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Synchronous as C
import Data.Char

-- test file
import Control.Monad.State
import Parse.Parser (parseProgram)
import Syntax.Expressions (ExpEnv)
import Syntax.Schemes (TypeEnv, VarEnv)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (stdout)
import System.Process
import Utils.FreestState
import Utils.PreludeLoader (prelude)
import Validation.Rename (renameState)
import Validation.TypeChecking (typeCheck)
import Validation.BuildTypes


import Control.Exception
import Say

hasLocked :: String -> IO a -> IO a
hasLocked msg action =
  action `catches`
  [ Handler $ \exc@BlockedIndefinitelyOnMVar -> sayString ("[MVar]: " ++ msg) >> throwIO exc
  , Handler $ \exc@BlockedIndefinitelyOnSTM -> sayString ("[STM]: " ++ msg) >> throwIO exc
  ]
-- p ::= e | p | p | (a, b)p

-- data Proc =
--     Exp Expression
--   | Parallel Proc Proc
--   | ScopeRest ProgVar ProgVar Proc
  
data RuntimeV a
  = VUnit
  | VInt Int
  | VChar Char
  | VBool Bool
  | VPair (RuntimeV a, RuntimeV a)
  | VClosure ProgVar Expression -- Ctx
  | PrimitiveFun (RuntimeV a -> RuntimeV a)
  | VConstr ProgVar [[RuntimeV a]] -- change list; does not make sense
  | PureWrapper (IO (RuntimeV a))
  | Chan (ChannelEnd (RuntimeV a) (RuntimeV a))
  | VVar ProgVar -- just to send over channels (select)
--  deriving Show

type ChannelEnd a b = (C.Chan a, C.Chan b)
type Channel    a b = (ChannelEnd a b, ChannelEnd b a)


instance Show (RuntimeV a) where
  show (VUnit) = "()"
  show (VInt i) = show i
  show (VChar c) = show c
  show (VBool b) = show b
  show (VPair p) = show p
  show c@(VConstr x v) = showConstr c
  show (VClosure x e) = "<<VClosure " ++ show x ++ " " ++ show e ++ ">>"
  show (PrimitiveFun _) = "<<primitivefun>>"
  show (PureWrapper _) = "<<io action>>" -- show (unsafePerformIO io)
  show (Chan _) = "Skip"
  show (VVar x) = show x

showConstrList :: [RuntimeV a] -> String
showConstrList xs = intercalate " " (map showC xs)

showC :: RuntimeV a -> String
showC c@(VConstr _ []) = show c
showC c@(VConstr _ _) = "(" ++ show c ++ ")"
showC v = show v

showConstr :: RuntimeV a -> String
showConstr (VConstr x []) = show x
showConstr (VConstr x xs) = show x ++ " " ++ (intercalate " " (map showConstrList xs))

type Ctx = Map.Map ProgVar Expression
type CtxBuiltin a = Map.Map ProgVar (RuntimeV a)
type IState = State Ctx

expandContext :: String -> Ctx -> ProgVar -> RuntimeV a -> Ctx
expandContext s ctx x e = do
  Map.insert x (fromRuntimeValue s e) ctx


evalProgVar :: CtxBuiltin a -> Ctx -> ProgVar
  -> IO (RuntimeV a, CtxBuiltin a, Ctx)
evalProgVar ctxB ctx x
  | Map.member x ctxB  = pure $ (ctxB Map.! x, ctxB, ctx)
  | isADT x            = pure $ (VConstr x [], ctxB, ctx)
  | otherwise          = eval ctxB ctx $ evalVar ctx x
  where
    evalVar :: Ctx -> ProgVar -> Expression
    evalVar ctx x
      | Map.member x ctx  = ctx Map.! x
      | otherwise = error $ "Trying to find " ++ show x ++ "\n" ++ showEnv ctx ++ "\n"

showEnv :: Ctx -> String
showEnv ctx = "\n--------------------\n" ++ (intercalate "\n\n") (map show (Map.toList (Map.difference ctx builtIn))) ++ "\n--------------------\n"

eval :: CtxBuiltin a -> Ctx -> Expression -> IO (RuntimeV a, CtxBuiltin a, Ctx)
eval ctxB ctx (Unit _) = pure $ (VUnit, ctxB, ctx)
eval ctxB ctx (Integer _ i) = pure $ (VInt i, ctxB, ctx)
eval ctxB ctx (Boolean _ b) = pure $ (VBool b, ctxB, ctx)
eval ctxB ctx (Character _ b) = pure $ (VChar b, ctxB, ctx)
eval ctxB ctx (ProgVar _ x) = evalProgVar ctxB ctx x
eval ctxB ctx (TypeApp _ x _) = evalProgVar ctxB ctx x
eval ctxB ctx (Lambda _ _ x _ e) = pure $ (VClosure x e, ctxB, ctx)
eval ctxB ctx (Pair _ e1 e2) = do
  (v1, ctxB1, _) <- eval ctxB ctx e1
  (v2, ctxB2, _) <- eval ctxB1 ctx e2
  return (VPair (v1, v2), ctxB2, ctx)
  
eval ctxB ctx (BinLet _ x y e1 e2) = do
  eval ctxB ctx e1 >>= \case
    (VPair (Chan a, Chan b), ctxB, _) ->
      eval (Map.insert x (Chan a) (Map.insert y (Chan b) ctxB)) ctx e2

    (VPair (v1, Chan b), ctxB, _) -> 
      let ctx1 = expandContext "" ctx x v1 in
      eval (Map.insert y (Chan b) ctxB) ctx1 e2

    (PureWrapper io, ctxB, _) -> do
      io >>= \case
        (VPair (Chan a, Chan b)) ->
          eval (Map.insert x (Chan a) (Map.insert y (Chan b) ctxB)) ctx e2
        (VPair (v1, Chan b)) -> do
          let ctx1 = expandContext "" ctx x v1
          eval (Map.insert y (Chan b) ctxB) ctx1 e2

    (VPair (v1,v2), ctxB, _) -> do
      let ctx1 = expandContext "a" (expandContext "a" ctx x v1) y v2
      eval ctxB ctx1 $ subExpr "a" (subExpr "a" e2 x v1) y v2     
    p ->   
      error $ "Pattern failed on bin let. expected pair; found: " ++ show p
    
eval ctxB ctx (UnLet _ x e1 e2) = do -- TODO: check with bin let
  eval ctxB ctx e1 >>= \case  
    (PureWrapper io, ctxB, _) -> do
      io >>= \case
        (Chan a) -> eval (Map.insert x (Chan a) ctxB) ctx e2
        v ->   
          let ctx1 = expandContext "UNLET1" ctx x v in
          eval ctxB ctx1 $ subExpr "UNLET1" e2 x v
    (Chan a, ctxB, _) -> eval (Map.insert x (Chan a) ctxB) ctx e2

    (v, ctxB, _)  -> do      
      let ctx1 = expandContext "UNLET" ctx x v 
      eval ctxB ctx1 $ subExpr "UNLET" e2 x v
  
eval ctxB ctx a@(App _ e1 e2) =
  eval ctxB ctx e1 >>= \case
    (VClosure x e, ctxB, ctx) -> -- do
      eval ctxB ctx e2 >>= \case
        (Chan a, ctxB, _) ->
          eval (Map.insert x (Chan a) ctxB) ctx e
        (v, ctxB, _) -> do
          let ctx1 = expandContext "APP" ctx x v
          eval ctxB ctx1 (subExpr "APP" e x v)
      
    (PrimitiveFun f, ctxB, ctx) -> do
      (v, ctxB, _) <- eval ctxB ctx e2
      case f v of
        (PureWrapper res) -> do
          !r <- res
          pure $ (r, ctxB, ctx)
        _ -> pure $ (f v, ctxB, ctx)
      
    (VConstr x xs, ctxB, ctx) -> do
      (v, ctxB, _) <- eval ctxB ctx e2
--      traceM $ show xs ++ " " ++ show v 
      pure $ (VConstr x (xs ++ [[v]]), ctxB, ctx)

eval ctxB ctx (Conditional _ b e1 e2) = do
  (VBool b', ctxB, _) <- eval ctxB ctx b
  if b' then eval ctxB ctx e1 else eval ctxB ctx e2

eval ctxB ctx (Case _ e m) = do
  (v, ctxB, _) <- eval ctxB ctx e
--  traceM $ "on case: " ++show v
  a@(v, _, ctx) <- evalCase ctxB ctx v m

  -- traceM $ "\nevalCase -> " ++ show v ++ "\n" 
  --       ++ "\nx " ++ show (ctx Map.!? (mkVar defaultPos "x")) ++ "\n"
  --       ++ "\nl " ++ show (ctx Map.!? (mkVar defaultPos "l")) ++ "\n"
  --       ++ "\nr " ++ show (ctx Map.!? (mkVar defaultPos "r")) ++ "\n"

  
  return a

eval ctxB ctx (Fork _ e) =
  return $ (PureWrapper $ do
    forkIO (eval ctxB ctx e >> return ()) >> return VUnit, ctxB, ctx)

eval ctxB ctx (New _ _ _) =
  return $ (PureWrapper $ do
    (ce1, ce2) <- _new
    return $ VPair (Chan ce1, Chan ce2), ctxB, ctx)
--    return $ VPair (Chan ce1, Chan ce2), ctxB, ctx)

eval ctxB ctx (Send _ e) = do   
   eval ctxB ctx e >>= \case    
     (Chan ch, ctxB, ctx) -> return $
       (PrimitiveFun (\y ->
            PureWrapper $ liftM Chan (_send "send" ch y)), ctxB, ctx)
     (PureWrapper io, ctxB, ctx) -> 
       io >>= \(Chan ch) -> 
       return $ (PrimitiveFun (\y ->
            PureWrapper $ liftM Chan (_send "send" ch y)), ctxB, ctx)

eval ctxB ctx (Receive _ e) = do
   (Chan ch, ctxB, ctx) <- eval ctxB ctx e
   (v, ch) <- _receive "recv" ch
   return $ (VPair (v , Chan ch), ctxB, ctx)
   
eval ctxB ctx (Select _ e x) = do
  (Chan ch, ctxB, ctx) <- eval ctxB ctx e
  -- traceM $ "on select " ++ show (VVar x)
  ch <- _send "select" ch (VVar x)
  return $ (Chan ch, ctxB, ctx)
    
eval ctxB ctx (Match _ e m) = do
  (Chan c, ctxB, ctx) <- eval ctxB ctx e
  (VVar !v, !c) <- _receive "match" c 
  let (patterns:_, e) = m Map.! v
  let ctxB1 = Map.insert patterns (Chan c) ctxB
  eval ctxB1 ctx e


eval _ _ e = error $ show e

-- TODO:
-- | prints ...

evalCase :: CtxBuiltin a -> Ctx -> RuntimeV a -> FieldMap -> IO (RuntimeV a, CtxBuiltin a, Ctx)
evalCase ctxB ctx (VConstr x xs) m = do
 
  let !(patterns, e) = m Map.! x
--  traceM $ show e
  let lst = zip patterns xs
  let ctx1 =
        foldl (\acc (v,xs) -> Map.insert v (fromRuntimeValue "" (head xs)) acc) ctx lst

  -- traceM $ "\nevalCase -> " ++ show x ++ " " ++ show lst ++ "\n"
  --       ++ "\nx " ++ show (ctx1 Map.!? (mkVar defaultPos "x")) ++ "\n"
  --       ++ "\nl " ++ show (ctx1 Map.!? (mkVar defaultPos "l")) ++ "\n"
  --       ++ "\nr " ++ show (ctx1 Map.!? (mkVar defaultPos "r")) ++ "\n"
  
  eval ctxB ctx1 e

_new :: IO ((C.Chan a1, C.Chan a2), (C.Chan a2, C.Chan a1))
_new = do
--  traceM $ "created a channel"
  ch1 <- C.newChan
  ch2 <- C.newChan
  return ((ch1, ch2), (ch2, ch1))

-- _send :: String -> Channel a b -> a2 -> IO (Channel a b)
_send str ch x  = do
--  traceM $ "send call from (" ++ str ++ "); sending value " ++ show x
  -- writeChan (snd ch) x -- (unsafeCoerce x)
  hasLocked ("DEADLOCKED on sending") $
    C.writeChan (snd ch) x
--  traceM $ "send " ++ str ++ " " ++ show c ++ " " ++ show x
  return ch

_receive :: [Char] -> (C.Chan a, b) -> IO (a, (C.Chan a, b))
_receive str ch = do
--  traceM $ "receive call from (" ++ str ++ ");"
--  a <- tryReadChan (fst ch)
--  traceM $ "receive " ++ str ++ " " ++ show a
  hasLocked ("DEADLOCKED on receiving " ++ str) $ do
    a <- C.readChan (fst ch)
    return (a, ch)
--  return (getSuccess a, ch)
--  return (unsafeCoerce a, ch)
  
  
subExpr :: String -> Expression -> ProgVar -> RuntimeV a -> Expression
-- subExpr s e x (Chan _) = e 
subExpr s (Lambda a b c d e) x v = Lambda a b c d (subExpr s e x v)
subExpr s (App p e1 e2) x v = App p (subExpr s e1 x v) (subExpr s e2 x v)
subExpr s e@(ProgVar _ y) x v
  | x == y    = fromRuntimeValue s v
  | otherwise = e
subExpr _ e _ _ = e

fromRuntimeValue :: String -> RuntimeV a -> Expression
fromRuntimeValue _ (VUnit) = Unit defaultPos
fromRuntimeValue _ (VInt i) = Integer defaultPos i
fromRuntimeValue _ (VBool b) = Boolean defaultPos b
fromRuntimeValue s (VPair (v, u)) = Pair defaultPos (fromRuntimeValue s v) (fromRuntimeValue s u)
fromRuntimeValue s (VConstr x xs) = constrFromRuntime s (ProgVar defaultPos x) xs
fromRuntimeValue _ (VClosure x e) = Lambda defaultPos Un x (Basic defaultPos IntType) e
  -- error $ "converting a <<closure>> from runtime to expression " ++ s
-- fromRuntimeValue s (Chan _)  = ProgVar defaultPos $ mkVar defaultPos "c" -- error $ "Chan " ++ s
fromRuntimeValue s val = error $ "error converting from runtime to expression " ++ show val ++ " fun " ++ s


--  :: [RuntimeV a] ->
constrFromRuntime :: String -> Expression -> [[RuntimeV a]] -> Expression
constrFromRuntime s = foldl f
  where
    f :: Expression -> [RuntimeV a] -> Expression
    f e [] = e
    f e (x:xs) = f (App defaultPos e (fromRuntimeValue s x)) xs

-- SETUP

-- TODO: add the remaining cons
ctxBuiltin :: CtxBuiltin a
ctxBuiltin =
  Map.fromList
  [ (var "(+)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x + y)))  
  , (var "(-)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x - y)))
  , (var "(*)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x * y)))
  , (var "(/)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x `div` y)))
  , (var "div", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x `div` y)))
  , (var "rem", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x `rem` y)))
  , (var "(==)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x == y)))
  , (var "(<=)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x <= y)))  
  , (var "(>=)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x >= y)))  
  , (var "(&&)", PrimitiveFun (\(VBool x) -> PrimitiveFun (\(VBool y) -> VBool $ x && y)))
  , (var "(||)", PrimitiveFun (\(VBool x) -> PrimitiveFun (\(VBool y) -> VBool $ x || y)))
  , (var "negate", PrimitiveFun (\(VInt x) -> VInt $ negate x))
  , (var "not", PrimitiveFun (\(VBool x) -> VBool $ not x))
  , (var "chr", PrimitiveFun (\(VInt x) -> VChar $ chr x))
--  , (var "_send", (PrimitiveFun (\(Chan x) ->
--                   PrimitiveFun (\y -> PureWrapper $ liftM Chan (_send x y)))))
  ]

eenv = Map.fromList [(mkVar defaultPos "f", fromType $ (Basic defaultPos BoolType)),
                          (mkVar defaultPos "h", fromType $ (Basic defaultPos BoolType))]


t1 :: IO ()
t1 = do
  let s = parseDefs "" eenv "f = 2 + 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

 :: IO ()
t2 = do
  let s = parseDefs "" eenv "f = h 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t2")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t3 :: IO ()
t3 = do
  let s = parseDefs "" eenv "f = 21 + (k 1 2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t3")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

-- booleans


t4 :: IO ()
t4 = do
  let s = parseDefs "" eenv "f = 3 == 1 || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t4")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state
  
t5 :: IO ()
t5 = do
  let s = parseDefs "" eenv "f = 3 == three || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t5")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t6 :: IO ()
t6 = do
  let s = parseDefs "" eenv "f = 3 == (h 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t6")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t7 :: IO ()
t7 = do
  let s = parseDefs "" eenv "f = 3 == (s 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t7")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state
  

t8 :: IO ()
t8 = do
  let s = parseDefs "" eenv "f = 0 == (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t8")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t9 :: IO ()
t9 = do
  let s = parseDefs "" eenv "f = isZero (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t9")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


-- pair of integers

t10 :: IO ()
t10 = do
  let s = parseDefs "" eenv "f = (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t10")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


t11 :: IO ()
t11 = do
  let s = parseDefs "" eenv "f = h (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t11")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

-- bin let

t12 :: IO ()
t12 = do
  let s = parseDefs "" eenv "f = let (x, y) = h (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t12")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

-- un let

t13 :: IO ()
t13 = do
  let s = parseDefs "" eenv "f = let x = h (1, 22) in x"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t13")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


-- un let (twice)

t14 :: IO ()
t14 = do
  let s = parseDefs "" eenv "f = let x = fst (1, 22) in let y = snd (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t14")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t15 :: IO ()
t15 = do
  let s = parseDefs "" eenv "f = let (x, y) = (1, 22) in (y, x)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t15")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


-- conditional
t16 :: IO ()
t16 = do
  let s = parseDefs "" eenv "f = if 0 == 1 then True else False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t16")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t17 :: IO ()
t17 = do
  let s = parseDefs "" eenv "f = half10 12"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state



t18 :: IO ()
t18 = do
  let s = parseDefs "" eenv "f = half10 (2*3+(5-4)*6+4/2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

t19 :: IO ()
t19 = do
  let s = parseDefs "" eenv "f = ()"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


t20 :: IO ()
t20 = do
  let s = parseDefs "" eenv "f = Cons 2 (Cons 3 Nil)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


-- case
t21 :: IO ()
t21 = do
  let s = parseDefs "" eenv "f = case cons of {Cons x y -> 0, Nil -> -1}"
  let s = parseDefs "" eenv "f = case nil of {Cons x y -> 0, Nil -> -1}"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


t22 :: IO ()
t22 = do
  let s = parseDefs "" eenv "f = let x = Cons 2 (Cons 3 Nil) in case x of {Cons x y -> 0, Nil -> -1}"  
  -- let s = parseDefs "" eenv "f = let x = Nil in case x of {Cons x y -> 0, Nil -> -1}"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state


t23 :: IO ()
t23 = do
  let s = parseDefs "" eenv "f = recCall 1"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  putStrLn $ show res
  return ()
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

-- simple fork

t24 :: IO ()
t24 = do
  let s = parseDefs "" eenv "f = fork (1 + 1)"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  (res,_,_) <- eval ctxBuiltin state e
  case res of
    PureWrapper io -> io >>= \res -> putStrLn $ show res
    _              -> putStrLn $ show res
  return ()

testFromFile :: FilePath -> IO ()
testFromFile args = do
  s1 <- parseProgram args prelude
  when (hasErrors s1) (die $ getErrors s1)
  let s2 = execState renameState s1
  let s3 = execState solveTypeDecls s2
  when (hasErrors s3) (die $ getErrors s3)
  let s4 = execState typeCheck s3
  when (hasErrors s4) (die $ getErrors s4)
  (res,_,_) <- eval ctxBuiltin (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
  
  case res of
    PureWrapper io -> io >>= \res -> putStrLn $ show res
    _              -> putStrLn $ show res
  return ()

t25 :: IO ()
t25 = testFromFile "tmp/parse.fst"
  
tests :: IO ()
tests = do
  let x = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
           t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,
           t21,t22,t23,t24,t25
          ]
  sequence_ x




-- BUILTIN ENV

-- ENVIRONMENT DEFINITIONS


plus, xVar, yVar, zVar  :: ProgVar
plus = mkVar defaultPos "(+)"
xVar = mkVar defaultPos "x"
yVar = mkVar defaultPos "y"
zVar = mkVar defaultPos "z"

var :: String -> ProgVar
var  = mkVar defaultPos

intType :: Type
intType = Basic defaultPos IntType 

p' :: Pos
p' = defaultPos

-- builtin + eenv
builtIn :: Ctx
builtIn =
  Map.fromList
    [
      -- id
      -- h = λ x -> x 
      (var "h", Lambda p' Un xVar intType (ProgVar p' xVar))
      -- k = λ x -> λ y -> y
    , (var "k", Lambda p' Un xVar intType (Lambda p' Un yVar intType (ProgVar p' yVar)))
      -- s = λ x -> λ y -> x+y
    , (var "s", Lambda p' Un xVar intType (Lambda p' Un yVar intType
           ((App p' (App p' (ProgVar p' plus) (ProgVar p' xVar)) (ProgVar p' yVar)))))
      -- x = 3
    , (var "three", Integer p' 3) -- test with var three and env
     -- m = λ x -> λ y -> x-y
    , (var "m", Lambda p' Un xVar intType (Lambda p' Un yVar intType
           (App p' (App p' (ProgVar p' (var "(-)")) (ProgVar p' xVar)) (ProgVar p' yVar))))
     -- isZero = λ x -> x == 0
    , (var "isZero", Lambda p' Un xVar intType
        (App p' (App p' (ProgVar p' (var "(==)")) (Integer p' 0)) (ProgVar p' xVar)))
     -- fst = λ x -> let (a, b) = x in a
    , (var "fst", Lambda p' Un xVar intType
       (BinLet p' (var "a") (var "b") (ProgVar p' xVar) (ProgVar p' (var "a")))
     )
     -- snd = λ x -> let (a, b) = x in b
    , (var "snd", Lambda p' Un xVar intType
       (BinLet p' (var "a") (var "b") (ProgVar p' xVar) (ProgVar p' (var "b")))
     )
     -- half10 = λ x -> if x < 10 then x else f (x / 2)
    , (var "half10", Lambda p' Un xVar intType
       (Conditional p'
         (App p' (App p' (ProgVar p' (var "(<=)")) (ProgVar p' xVar)) (Integer p' 10))
         (ProgVar p' xVar)
         (App p' (ProgVar p' (var "half10"))
           (App p' (App p' (ProgVar p' (var "(/)")) (ProgVar p' xVar)) (Integer p' 2)))
      ))
    
   , (var "cons", (App p'
                      (App p' (ProgVar p' (var "Cons")) (Integer p' 2))
                      (App p'
                         (App p' (ProgVar p' (var "Cons")) (Integer p' 3))
                         (ProgVar p' (var "Nil"))))
     )
   , (var "nil", (ProgVar p' (var "Nil")))
   
   , (var "recCall", Lambda p' Un xVar intType
       (Conditional p'
         (App p' (App p' (ProgVar p' (var "(<=)")) (ProgVar p' xVar)) (Integer p' 10))
         (App p' (App p' (ProgVar p' (var "(*)")) (Integer p' 2))
                 (App p'
                   (ProgVar p' (var "recCall"))
                   (App p' (App p' (ProgVar p' (var "(+)"))
                           (Integer p' 1)) (ProgVar p' xVar))
                 ))
         (Integer p' 1)
        
      ))
    ]





