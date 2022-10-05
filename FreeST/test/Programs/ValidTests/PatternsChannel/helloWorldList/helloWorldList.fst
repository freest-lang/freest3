data List = Nil | Cons Char List

server : forall α : 1S . (rec x:1S.&{Done: Skip, More: ?Char;x});α -> (List, α)
server (Done c) = (Nil, c)
server (More c) =
      let (h, c) = receive c in
      let (t, c) = server@α c in
      (Cons h t, c)

client : forall α :1S . List -> (rec x:1S.+{Done: Skip, More: !Char;x});α -> α
client Nil c = select Done c
client (Cons h t) c =
--      client@α l (send cons (select More c))
      let c = select More c in
      let c = send h c in
      let c3 = client@α t c in
      c3

hello : List
hello = Cons 'H' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil))))

main : List
main =
  let (c, s) = new (rec x:1S.+{Done: Skip, More: !Char;x}) in
  fork (\_:() 1-> client@Skip hello c) ;
  let (res, c) = server@Skip s in
  res
