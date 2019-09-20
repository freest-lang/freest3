data List = Nil | Cons Char List

server : forall α : SL => (rec x:SL.&{Done: Skip, More: ?Char;x});α -> (List, α)
server c =
  match c with {
    More c -> 
      let (h, c) = receive c in
      let (t, c) = server[α] c in
      (Cons h t, c),
    Done c ->
      (Nil, c)
  }

client : forall α :SL => List -> (rec x:SL.+{Done: Skip, More: !Char;x});α -> α
client l c =
  case l of {
    Nil ->
      select c Done,
    Cons h t ->
--      client[α] l (send cons (select More c))
      let c = select c More in
      let c = send c h in
      let c3 = client[α] t c in
      c3
  }

hello : List
hello = Cons 'H' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil))))

main : List
main = 
  let (c, s) = new (rec x:SL.+{Done: Skip, More: !Char;x}) in
  let x = fork (client[Skip] hello c) in
  let (res, c) = server[Skip] s in 
  res

