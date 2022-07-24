{- |
Module      : Stack
Description : A stack with a type controlled by a context-free session type
Copyright   : (c) Vasco T. Vasconcelos, 11 apr 2021

Based on an example in
  Luca Padovani. Context-free session type inference.
  ACM Trans. Program. Lang. Syst., 41(2):9:1–9:37, 2019.
-}

type  EStack : 1S = &{Push: ?Int; NEStack; EStack,  End: Skip}

type NEStack : 1S = &{Push: ?Int; NEStack; NEStack, Pop: !Int}

eStack : ∀ a: 1S . EStack;a -> a
eStack c =
  case collect c of {
    Push c -> let (x, c) = receive c in eStack @a (neStack @(EStack ; a) x c),
    End  c -> c
  }

neStack : ∀ a: 1S . Int -> NEStack;a -> a
neStack x c =
  case collect c of {
    Push c -> let (y, c) = receive c in neStack @a x (neStack @(NEStack ; a) y c),
    Pop  c -> send x c
  }

aStackClient : dualof EStack -> Int
aStackClient c =
  let c = select Push c in let c      = send  5 c in
  let c = select Pop  c in let (_, c) = receive c in
  let c = select Push c in let c      = send  7 c in
  let c = select Push c in let c      = send  9 c in
  let c = select Push c in let c      = send 11 c in
  let c = select Push c in let c      = send 13 c in
  let c = select Pop  c in let (_, c) = receive c in
  let c = select Pop  c in let (_, c) = receive c in
  let c = select Pop  c in let (_, c) = receive c in
  let c = select Pop  c in let (x, c) = receive c in
  -- let c = select Pop  c in let (_, c) = receive c in
  -- Error: Branch Pop not present in internal choice type dualof EStack
  let _ = select End  c in x

main : Int
main =
  let (r, w) = new EStack in
  fork (\_:()1-> eStack @Skip r);
  aStackClient w
    
