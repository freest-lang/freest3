type FiniteStream = &{Done: Skip, More: ?Int;FiniteStream}

ints : ∀ c . Int -> dualof FiniteStream;c -> c
ints n c = 
    if n < 0
    then select Done c
    else select More c |> send n |> ints @c (n - 1)

type Fold = FiniteStream;!Int;End

foldClient : Int -> dualof Fold -> Int
foldClient n w = ints @(?Int;End) n w |> receiveAndClose @Int

foldServer : Int -> Fold -> ()
foldServer sum c =
    match c with {
        Done c -> send sum c |> close,
        More c -> let (n, c) = receive c in
                  foldServer (sum + n) c
    }

main : Int
main = 
    let (s, c) = new @Fold () in
    fork (\_:()1-> foldServer 0 s);
    foldClient 4 c
