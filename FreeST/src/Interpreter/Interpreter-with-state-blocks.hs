{-# LANGUAGE BangPatterns #-}
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
import Data.Maybe -- Debug
import Validation.Rename -- Debug
import Data.Char (isUpper)
import Parse.Parser
import Utils.FreestState
import Debug.Trace
import Text.Show.Functions
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan-- .Synchronous

-- test file
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
--  | Primitive (RuntimeValues -> RuntimeValues -> RuntimeValues)
--  deriving Show


instance Show (RuntimeV a) where
  show (VUnit) = "()"
  show (VInt i) = show i
  show (VChar c) = show c
  show (VBool b) = show b
  show (VPair p) = show p
  show c@(VConstr _ _) = showConstr c
  show (VClosure x e) = "<<VClosure " ++ show x ++ " " ++ show e ++ ">>"
  show (PrimitiveFun _) = "<<primitivefun>>"
  show (PureWrapper io) = "<<io action>>" -- show (unsafePerformIO io)
  show (Chan _) = "<<channel>>"

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

data InterpreterS a = InterpreterS
  {
    _builtIn :: CtxBuiltin a
  , _ctxt    :: Ctx
  }

type IState a = StateT (InterpreterS a) IO

initState :: CtxBuiltin a -> Ctx -> InterpreterS a
initState bin ctx = InterpreterS {
  _builtIn  = bin
, _ctxt     = ctx
--   _builtIn  = Map.empty
-- , _ctxt    = Map.empty
}

-- expandContext :: Ctx -> ProgVar -> RuntimeV a -> IState ()
expandContext :: ProgVar -> (RuntimeV a) -> (IState a) ()
expandContext x e =
  modify (\s -> s{_ctxt = Map.insert x (fromRuntimeValue e) (_ctxt s)})

expandBuiltin :: ProgVar -> (RuntimeV a) -> (IState a) ()
expandBuiltin x v =
  modify (\s -> s{_builtIn = Map.insert x v (_builtIn s)})  

builtInMember :: ProgVar -> (IState a) (Maybe (RuntimeV a))
builtInMember x = do
  s <- get
  return $ (_builtIn s) Map.!? x

ctxMember :: ProgVar -> (IState a) (Maybe Expression)
ctxMember x = do
  s <- get
  return $ (_ctxt s) Map.!? x  
  
--  s <- get
--  let env = Map.insert x (fromRuntimeValue e) s
--  put env
evalProgVar :: ProgVar -> (IState a) (RuntimeV a)
evalProgVar x = do
  bin <- builtInMember x
  ctx <- ctxMember x
  evalVar bin (isADT x) ctx
  where
    evalVar :: Maybe (RuntimeV a) -> Bool -> Maybe Expression -> (IState a) (RuntimeV a)
    evalVar (Just bin) _ _ = return bin
    evalVar _ True _       = return $ VConstr x []  
    evalVar _ _ (Just e)   = eval e

-- showEnv :: Ctx -> String
-- showEnv ctx =  (intercalate "\n") (map show (Map.toList (Map.difference ctx builtIn)))

  

-- eval :: CtxBuiltin a -> Ctx -> Expression -> IO (RuntimeV a)
eval :: Expression -> (IState a) (RuntimeV a)
eval (Unit _) = pure $ VUnit
eval (Integer _ i) = pure $ VInt i
eval (Boolean _ b) = pure $ VBool b
eval (ProgVar _ x) = evalProgVar x
eval (TypeApp _ x _) = evalProgVar x
eval (Lambda _ _ x _ e) = pure $ VClosure x e
eval (Pair _ e1 e2) =
  liftM VPair $ liftM2 ((,)) (eval e1) (eval e2)
  
eval (BinLet _ x y e1 e2) = do
  eval e1 >>= \case
    (VPair (Chan a, Chan b)) -> do
      expandBuiltin x (Chan a)
      expandBuiltin y (Chan b)
      eval e2

    (VPair (v1, Chan b)) -> do
     expandContext x v1
     expandBuiltin y (Chan b)
     eval e2

    (PureWrapper io) -> do
      liftIO io >>= \case
        (VPair (Chan a, Chan b)) -> do
          expandBuiltin x (Chan a)
          expandBuiltin y (Chan b)
          eval e2
        (VPair (v1, Chan b)) -> do
          expandContext x v1
          expandBuiltin y (Chan b)
          eval e2

    (VPair (v1,v2)) -> do
      expandContext x v1
      expandContext y v2
      eval (subExpr (subExpr e2 x v1) y v2)
    p ->   
      error $ "Pattern failed on bin let. expected pair; found: " ++ show p
    
eval (UnLet _ x e1 e2) = do -- TODO: check with bin let & refactor
  eval e1 >>= \case
    (PureWrapper io) -> do
      liftIO io >>= \case
        (Chan a) -> expandBuiltin x (Chan a) >> eval e2
        v -> expandContext x v >> eval (subExpr e2 x v)
        
    (Chan a) -> expandBuiltin x (Chan a) >> eval e2
    v -> expandContext x v >> eval (subExpr e2 x v)
      
eval (App _ e1 e2) =
  eval e1 >>= \case
    (VClosure x e) -> -- do
      eval e2 >>= \case
        c@(Chan _) -> expandBuiltin x c >> eval e
        v -> expandContext x v >> eval (subExpr e x v)
      
    (PrimitiveFun f) -> do
      v <- eval e2
      case f v of
        (PureWrapper res) -> do
          liftIO $ do
            print $ "aa"
            !r <- res
            print $ show r
          --pure $
          liftIO res
        _ -> pure $ f v
      
    (VConstr x xs) -> do
      v <- eval e2
      pure $ VConstr x (xs ++ [[v]])

--     _ -> error "APP ERROR"

eval (Conditional _ b e1 e2) = do
  (VBool b') <- eval b
  if b' then eval e1 else eval e2

eval (Case _ e m) = do
  v <- eval e
  evalCase v m

eval (Fork _ e) =
  return $ PureWrapper $ 
    forkIO (return (eval e) >> return ()) >> return VUnit

eval (New _ _ _) =
--  return $ PureWrapper $ liftM Chan _new
  return $ PureWrapper $ do
    (ce1, ce2) <- _new
    return $ VPair (Chan ce1, Chan ce2)

eval (Send _ e) = do
   (Chan ch) <- eval e
   return $ (PrimitiveFun (\y -> PureWrapper $ liftM Chan (_send ch y)))

eval (Receive _ e) = do
   (Chan ch) <- eval e
   (v, ch) <- liftIO $ _receive ch
   return $ VPair (v , Chan ch)

  

-- eval _ _ e = error $ show e

-- -- TODO:

-- -- | Select Pos Expression ProgVar
-- -- | Match Pos Expression FieldMap
-- -- | prints ...

-- -- TEST
-- -- | TypeApp Pos ProgVar [Type] 
-- -- | Fork Pos Expression
-- -- | New Pos Type Type
-- -- | Send Pos Expression
-- -- | Receive Pos Expression

evalCase :: RuntimeV a -> FieldMap -> (IState a) (RuntimeV a)
evalCase (VConstr x xs) m = do
  let !(patterns, e) = m Map.! x
  -- TODO: add to env
  eval e
  
--   -- case v of
--   --   VConstr x []

_new = do
  ch1 <- newChan
  ch2 <- newChan
  return ((ch1, ch2), (ch2, ch1))
  
_send ch x  = do
  writeChan (snd ch) x -- (unsafeCoerce x)
  return ch

_receive ch = do
  a <- readChan (fst ch)
  return (a, ch)
--  return (unsafeCoerce a, ch)
  
  
subExpr :: Expression -> ProgVar -> RuntimeV a -> Expression
subExpr (Lambda a b c d e) x v = Lambda a b c d (subExpr e x v)
subExpr (App p e1 e2) x v = App p (subExpr e1 x v) (subExpr e2 x v)
-- subExpr (Send _ y) x v = subExpr y x v -- ??
subExpr e@(ProgVar _ y) x v
  | x == y    = fromRuntimeValue v
  | otherwise = e
subExpr e _ _ = e

fromRuntimeValue :: RuntimeV a -> Expression
fromRuntimeValue (VUnit) = Unit defaultPos
fromRuntimeValue (VInt i) = Integer defaultPos i
fromRuntimeValue (VBool b) = Boolean defaultPos b
fromRuntimeValue (VPair (v, u)) = Pair defaultPos (fromRuntimeValue v) (fromRuntimeValue u)
fromRuntimeValue (VConstr x xs) = constrFromRuntime (ProgVar defaultPos x) xs
fromRuntimeValue (VClosure x e) = Lambda defaultPos Un x (Basic defaultPos IntType) e
  -- error $ "converting a <<closure>> from runtime to expression " ++ s
-- fromRuntimeValue s (Chan _)  = ProgVar defaultPos $ mkVar defaultPos "c" -- error $ "Chan " ++ s
fromRuntimeValue val = error $ "error converting from runtime to expression " ++ show val


-- --  :: [RuntimeV a] ->
constrFromRuntime :: Expression -> [[RuntimeV a]] -> Expression
constrFromRuntime = foldl f
  where
    f :: Expression -> [RuntimeV a] -> Expression
    f e [] = e
    f e (x:xs) = f (App defaultPos e (fromRuntimeValue x)) xs

-- -- SETUP

type ChannelEnd a b = (Chan a, Chan b)
type Channel    a b = (ChannelEnd a b, ChannelEnd b a)

-- -- TODO: add the remaining cons
ctxBuiltin :: CtxBuiltin a
ctxBuiltin =
  Map.fromList
  [ (var "(+)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x + y)))  
  , (var "(-)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x - y)))
  , (var "(*)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x * y)))
  , (var "(/)" , PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x `div` y)))
  , (var "div", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VInt $ x `div` y)))
  , (var "(==)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x == y)))
  , (var "(<=)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x <= y)))  
  , (var "(>=)", PrimitiveFun (\(VInt x) -> PrimitiveFun (\(VInt y) -> VBool $ x >= y)))  
  , (var "(&&)", PrimitiveFun (\(VBool x) -> PrimitiveFun (\(VBool y) -> VBool $ x && y)))
  , (var "(||)", PrimitiveFun (\(VBool x) -> PrimitiveFun (\(VBool y) -> VBool $ x || y)))
  , (var "negate", PrimitiveFun (\(VInt x) -> VInt $ negate x))
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
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

t2 :: IO ()
t2 = do
  let s = parseDefs "" eenv "f = h 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  
t3 :: IO ()
t3 = do
  let s = parseDefs "" eenv "f = 21 + (k 1 2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  -- putStrLn $ show $ fst $ runState (eval ctxBuiltin e) state

-- -- booleans


t4 :: IO ()
t4 = do
  let s = parseDefs "" eenv "f = 3 == 1 || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  
t5 :: IO ()
t5 = do
  let s = parseDefs "" eenv "f = 3 == three || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  
t6 :: IO ()
t6 = do
  let s = parseDefs "" eenv "f = 3 == (h 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

t7 :: IO ()
t7 = do
  let s = parseDefs "" eenv "f = 3 == (s 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  

t8 :: IO ()
t8 = do
  let s = parseDefs "" eenv "f = 0 == (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

t9 :: IO ()
t9 = do
  let s = parseDefs "" eenv "f = isZero (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


-- pair of integers

t10 :: IO ()
t10 = do
  let s = parseDefs "" eenv "f = (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


t11 :: IO ()
t11 = do
  let s = parseDefs "" eenv "f = h (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

-- bin let

t12 :: IO ()
t12 = do
  let s = parseDefs "" eenv "f = let (x, y) = h (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

-- un let

t13 :: IO ()
t13 = do
  let s = parseDefs "" eenv "f = let x = h (1, 22) in x"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


-- un let (twice)

t14 :: IO ()
t14 = do
  let s = parseDefs "" eenv "f = let x = fst (1, 22) in let y = snd (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

t15 :: IO ()
t15 = do
  let s = parseDefs "" eenv "f = let (x, y) = (1, 22) in (y, x)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


-- -- conditional
t16 :: IO ()
t16 = do
  let s = parseDefs "" eenv "f = if 0 == 1 then True else False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s
  
t17 :: IO ()
t17 = do
  let s = parseDefs "" eenv "f = half10 12"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s



t18 :: IO ()
t18 = do
  let s = parseDefs "" eenv "f = half10 (2*3+(5-4)*6+4/2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

t19 :: IO ()
t19 = do
  let s = parseDefs "" eenv "f = ()"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


t20 :: IO ()
t20 = do
  let s = parseDefs "" eenv "f = Cons 2 (Cons 3 Nil)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


-- case
t21 :: IO ()
t21 = do
  let s = parseDefs "" eenv "f = case cons of {Cons x y -> 0, Nil -> -1}"
  let s = parseDefs "" eenv "f = case nil of {Cons x y -> 0, Nil -> -1}"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


t22 :: IO ()
t22 = do
  let s = parseDefs "" eenv "f = let x = Cons 2 (Cons 3 Nil) in case x of {Cons x y -> 0, Nil -> -1}"  
  -- let s = parseDefs "" eenv "f = let x = Nil in case x of {Cons x y -> 0, Nil -> -1}"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s


t23 :: IO ()
t23 = do
  let s = parseDefs "" eenv "f = recCall 1"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (s, _) <- runStateT (eval e) (initState ctxBuiltin state)
  putStrLn $ show s

-- simple fork

t24 :: IO ()
t24 = do
  let s = parseDefs "" eenv "f = fork (1 + 1)"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  (res, _) <- runStateT (eval e) (initState ctxBuiltin state)
  case res of
    PureWrapper io -> io >>= \res -> putStrLn $ show res
    _              -> putStrLn $ show res


testFromFile :: FilePath -> IO ()
testFromFile args = do
  s1 <- parseProgram args prelude
  let s2 = execState renameState s1
  let s3 = execState solveTypeDecls s2
  let s4 = execState typeCheck s3
  let main = (expEnv s4) Map.! (mkVar defaultPos "main")
  !(!res, _) <- runStateT (eval main) (initState ctxBuiltin (expEnv s4))
  case res of
    PureWrapper io -> io >>= \res -> putStrLn $ show res
    _              -> putStrLn $ show res
  return ()

t25 :: IO ()
t25 = testFromFile "tmp/parse.fst"
  
tests :: IO ()
tests = do
  let x = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10
          ,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20
          ,t21,t22,t23,t24,t25
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

-- -- builtin + eenv
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





