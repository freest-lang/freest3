data List = Nil | Cons Char List
type OutCharStream = +{Done: Skip, More: !Char;OutCharStream}

client : List -> OutCharStream;α -> α
client l c =
  case l of {
    Nil ->
      select Done c,
    Cons h t ->
--      client[α] l (send cons (select More c))
      let c = select More c in
      let c = send h c in
      let c3 = client @α t c in
      c3
  }

hello : List

hello = Cons 'H' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil))))
main : ()
main = 
  newHcClient @(OutCharStream;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  client @Close hello |> 
  close
