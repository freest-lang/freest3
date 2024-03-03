data List = Nil | Cons Char List
type InCharStream = &{Done: Skip, More: ?Char;InCharStream}
type OutCharStream = dualof InCharStream

-- server : forall α : 1S . InCharStream;α -> (List, α)
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

hello, main : List

hello = Cons 'H' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil))))

main = 
  let (c, s) = new @(OutCharStream;Close) () in
  let x = fork @() (\_:()1-> client @Close hello c |> close) in
  let (res, c) = server @Wait s in
  wait c;
  res
