data List = Nil | Cons Int List

type SendList = +{Nil: Skip, Cons: !Int;SendList}

flatten : List -> SendList;a -> a
flatten l c =
  case l of {
    Nil -> select Nil c,
    Cons h t ->
      let c = select Cons c in
      let c = send h c in
      flatten @a t c
  }

aList : List
aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main : ()
main =
  newHcClient @(SendList;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  flatten @Close aList |> 
  close
