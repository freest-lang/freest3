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

type  EStack = +{Push: !Int; NEStack; EStack,  Stop: Skip}
type NEStack = +{Push: !Int; NEStack; NEStack, Pop : ?Int}


-- Stack operations. Push on an empty stack
pushE : Int ->  EStack ; a ->  NEStack ;  EStack ; a
pushE n c = select Push c |> send n

-- Stack operations. Push on a nonempty stack
pushNE : Int ->  NEStack ; a ->  NEStack ;  NEStack ; a
pushNE n c = select Push c |> send n

-- Stack operations. Pop from a nonempty stack (and print the result)
pop :  NEStack;a -> a
pop c = 
  let c = select Pop c in let (x, c) = receive c in
  putStr (show @Int x) ; putStr " " ; c

-- A finite client
reverseThree :  EStack -> Skip
reverseThree c =
  pushE   @Skip 5 c
  |> pushNE  @( EStack) 6
  |> pushNE  @( NEStack ;  EStack) 7
  |> pop     @( NEStack ;  NEStack ;  EStack)
  |> pop     @( NEStack ;  EStack)
  |> pop     @( EStack)
  |> select Stop

-- A recursive client working on a nonempty stack
reverseNE : Int ->  NEStack ; a ->  NEStack ; a
reverseNE n c =
  if n == 0
  then c
  else
    pushNE  @a n c
    |> reverseNE  @( NEStack ; a) (n - 1)
    |> pop  @( NEStack ; a)

-- A generic client working on an empty stack
reverseE : Int ->  EStack;Close -> ()
reverseE n c =
  pushE @Close n c
  |> reverseNE  @( EStack;Close) (n-1)
  |> pop  @( EStack;Close)
  |> select Stop
  |> close

main : ()
main =
  newHcClient1 @(EStack;Close) ("127.0.0.1", "8081") |>
  reverseE 10
  -- reverseThree w

