{- |
Module      : Stack
Description : A stack with a type controlled by a context-free session type
Copyright   : (c) Vasco T. Vasconcelos, 11 apr 2021

Based on an example in
  Luca Padovani. Context-free session type inference.
  ACM Trans. Program. Lang. Syst., 41(2):9:1–9:37, 2019.
-}

Stack  = & {  Push: ?Int ; Stack, 
              Pop: Skip, 
              Peek: !Int ; Stack} ; Close

type EStack  = +{Push: !Int; NEStack ; EStack,  Stop: Skip}

type NEStack = +{Push: !Int; NEStack ; NEStack, Pop: ?Int}

aStackClient : EStack ; Close -> Int
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
  select Stop  c |> close;
  x 

main : Int
main =
  newHcClient1 @(EStack ; Close) ("127.0.0.1", "8081") |>
  aStackClient
    
