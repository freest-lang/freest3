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

type  EStack : 1S = &{Push: ?Int; NEStack; EStack,  Stop: Skip}
type NEStack : 1S = &{Push: ?Int; NEStack; NEStack, Pop: !Int}

-- Stack server. The empty stack case
eStack : forall a: 1S . EStack;a -> a
eStack c =
  match c with {
    Push c -> let (x, c) = receive c in eStack @a (neStack @(EStack ; a) x c),
    Stop  c -> c
  }

-- Stack server. The non-empty stack case
neStack : forall a: 1S . Int -> NEStack;a -> a
neStack x c =
  match c with {
    Push c -> let (y, c) = receive c in neStack @a x (neStack @(NEStack ; a) y c),
    Pop  c -> send x c
  }

-- Stack operations. Push on an empty stack
pushE : forall a: 1S . Int -> dualof EStack ; a -> dualof NEStack ; dualof EStack ; a
pushE n c = select Push c & send n

-- Stack operations. Push on a nonempty stack
pushNE : forall a: 1S . Int -> dualof NEStack ; a -> dualof NEStack ; dualof NEStack ; a
pushNE n c = select Push c & send n

-- Stack operations. Pop from a nonempty stack (and print the result)
pop : forall a: 1S . dualof NEStack;a -> a
pop c = 
  let c = select Pop c in let (x, c) = receive c in
  printInt x ; printString " " ; c

-- A finite client
reverseThree : dualof EStack -> Skip
reverseThree c =
  pushE   @Skip 5 c &
  pushNE  @(dualof EStack) 6 &
  pushNE  @(dualof NEStack ; dualof EStack) 7 &
  pop     @(dualof NEStack ; dualof NEStack ; dualof EStack) &
  pop     @(dualof NEStack ; dualof EStack) &
  pop     @(dualof EStack) &
  select Stop

-- A recursive client working on a nonempty stack
reverseNE : forall a: 1S . Int -> dualof NEStack ; a -> dualof NEStack ; a
reverseNE n c =
  if n == 0
  then c
  else
    pushNE  @a n c &
    reverseNE  @(dualof NEStack ; a) (n - 1) &
    pop  @(dualof NEStack ; a)

-- A generic client working on an empty stack
reverseE : Int -> dualof EStack -> Skip
reverseE n c =
  pushE  @Skip n c &
  reverseNE  @(dualof EStack) (n-1) &
  pop  @(dualof EStack) &
  select Stop

main : Skip
main =
  let (r, w) = new EStack in
  fork  @Skip $ eStack  @Skip r;
  reverseE 10 w
  -- reverseThree w

