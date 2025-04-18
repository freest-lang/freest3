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

-- Stack operations. Push on an empty stack
pushE : Int -> dualof EStack ; a -> dualof NEStack ; dualof EStack ; a
pushE n c = select Push c |> send n

-- Stack operations. Push on a nonempty stack
pushNE : Int -> dualof NEStack ; a -> dualof NEStack ; dualof NEStack ; a
pushNE n c = select Push c |> send n

-- Stack operations. Pop from a nonempty stack (and print the result)
pop : dualof NEStack;a -> a
pop c = 
  let c = select Pop c in let (x, c) = receive c in
  putStr (show @Int x) ; putStr " " ; c

-- A finite client
reverseThree : dualof EStack -> Skip
reverseThree c =
  pushE   @Skip 5 c
  |> pushNE  @(dualof EStack) 6
  |> pushNE  @(dualof NEStack ; dualof EStack) 7
  |> pop     @(dualof NEStack ; dualof NEStack ; dualof EStack)
  |> pop     @(dualof NEStack ; dualof EStack)
  |> pop     @(dualof EStack)
  |> select Stop

-- A recursive client working on a nonempty stack
reverseNE : Int -> dualof NEStack ; a -> dualof NEStack ; a
reverseNE n c =
  if n == 0
  then c
  else
    pushNE  @a n c
    |> reverseNE  @(dualof NEStack ; a) (n - 1)
    |> pop  @(dualof NEStack ; a)

-- A generic client working on an empty stack
reverseE : Int -> dualof EStack;Close -> ()
reverseE n c =
  pushE @Close n c
  |> reverseNE  @(dualof EStack;Close) (n-1)
  |> pop  @(dualof EStack;Close)
  |> select Stop
  |> close

main : ()
main =
  let (r, w) = new @(EStack;Wait) () in
  fork  @() (\_:()1-> eStack @Wait r |> wait);
  reverseE 10 w
  -- reverseThree w

