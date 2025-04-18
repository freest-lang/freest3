type FiniteStream = +{Done: Skip, More: !Int;FiniteStream}
type Fold = FiniteStream;?Int;Close

ints : Int -> FiniteStream; c -> c
ints n c = 
    if n < 0
    then select Done c
    else select More c |> send n |> ints @c (n - 1)

foldClient : Int -> Fold -> Int
foldClient n w = ints @(?Int;Close) n w |> receiveAndClose @Int

main : Int
main = 
    newHcClient1 @Fold ("127.0.0.1", "8081") |>
    foldClient 4
