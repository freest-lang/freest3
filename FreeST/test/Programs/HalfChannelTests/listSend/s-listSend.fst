data List = Nil | Cons Int List

type RecvList = &{Nil: Skip, Cons: ?Int;RecvList}

reconstruct : RecvList;a -> (List, a)
reconstruct c =
  match c with {
    Nil c -> (Nil, c),
    Cons c ->
      let (h, c) = receive c in
      let (t, c) = reconstruct @a c in
      (Cons h t, c)
  }

main : List
main =
  let (l, c) = newHcServer @(RecvList;Wait) ("127.0.0.1", "8081") |>
  reconstruct @Wait in 
  wait c;
  l
