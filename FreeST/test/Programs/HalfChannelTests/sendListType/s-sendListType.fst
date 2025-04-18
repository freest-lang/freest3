data List = Cons Int List | Nil

type ListOut = &{NilC: Skip, ConsC: ?Int ; ListOut}

rcvList : ListOut;a -> (List, a)
rcvList c =
  match c with {
    ConsC c  ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    NilC c  -> (Nil, c)
  }

main : List
main =
  let (l, c) = newHcServer @(ListOut;Wait) ("127.0.0.1", "8081") |>
  rcvList @Wait in
  wait c;
  l
