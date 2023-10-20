data List = Nil | Cons Char List
type InCharStream = &{Done: Skip, More: ?Char;InCharStream}
type OutCharStream = dualof InCharStream

server : forall α . InCharStream;α -> (List, α)
server (Done c) = (Nil, c)
server (More c) =
      let (h, c) = receive c in
      let (t, c) = server@α c in
      (Cons h t, c)

client : forall α . List -> OutCharStream;α -> α
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
      let (c, s) = new @(OutCharStream;End) () in
      let x = fork @() (\_:()1-> client @End hello c |> close) in
      let (res, c) = server @End s in
      close c;
      res
