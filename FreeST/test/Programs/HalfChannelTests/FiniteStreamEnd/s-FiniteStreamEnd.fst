type FiniteStream = &{Done: Skip, More: ?Int;FiniteStream}

type Fold = FiniteStream;!Int;Wait

foldServer : Int -> Fold -> ()
foldServer sum c =
    match c with {
        Done c -> send sum c |> wait,
        More c -> let (n, c) = receive c in
                  foldServer (sum + n) c
    }

main : ()
main = 
    newHcServer @Fold ("127.0.0.1", "8081") |>
    foldServer 0
