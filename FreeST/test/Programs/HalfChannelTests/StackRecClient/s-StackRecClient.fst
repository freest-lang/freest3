{- |
Module      : Stack
Description : A stack with a type controlled by a context-free session type
Copyright   : (c) Vasco T. Vasconcelos, 1 Nov 2021

Based on an example in
  Luca Padovani. Context-free session type inference.
  ACM Trans. Program. Lang. Syst., 41(2):9:1-9:37, 2019.

Features a recursive client that reverses the list [10..1]. Notice two functions
with "exact" the same code but different types: ePush and pushNE. The code seems
exactly the same, but in fact 'select Push' works on two distinct types: EStack
and NEStack. They both feature a Push-labelled field.
-}

type  EStack = &{Push: ?Int; NEStack; EStack,  Stop: Skip}
type NEStack = &{Push: ?Int; NEStack; NEStack, Pop: !Int}

-- Stack server. The non-empty stack case
neStack : Int -> NEStack;a -> a
neStack x c =
  match c with {
    Push c -> let (y, c) = receive c in neStack @a x (neStack @(NEStack ; a) y c),
    Pop  c -> send x c
  }

-- Stack server. The empty stack case
eStack : EStack;a -> a
eStack c =
  match c with {
    Push c -> let (x, c) = receive c in eStack @a (neStack @(EStack ; a) x c),
    Stop  c -> c
  }

main : ()
main =
  newHcServer @(EStack;Wait) ("127.0.0.1", "8081") |>
  eStack @Wait |> wait

