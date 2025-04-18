data List = Nil | Cons Char List
type InCharStream = &{Done: Skip, More: ?Char;InCharStream}

-- server : forall α . InCharStream;α -> (List, α)
server : InCharStream;α -> (List, α)
server c =
  match c with {
    More c ->
      let (h, c) = receive c in
      let (t, c) = server @α c in
      (Cons h t, c),
    Done c ->
      (Nil, c)
  }

hello, main : List

hello = Cons 'H' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil))))

main = 
  let (res, c1) = newHcServer @(InCharStream;Wait) ("127.0.0.1", "8081") |>
  server @Wait in
  wait c1;
  res
