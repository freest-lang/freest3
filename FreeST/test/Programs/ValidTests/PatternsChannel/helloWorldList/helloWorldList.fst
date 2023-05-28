data List = Nil | Cons Char List
type InCharStream  : 1S = &{Done: Skip, More: ?Char;InCharStream}
type OutCharStream : 1S = dualof InCharStream

server : forall α : 1S . InCharStream;α -> (List, α)
server (Done c) = (Nil, c)
server (More c) =
      let (h, c) = receive c in
      let (t, c) = server@α c in
      (Cons h t, c)

client : forall α :1S . List -> OutCharStream;α -> α
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
      let (c, s) = new @(OutCharStream;EndC) () in
      let x = fork @() (\_:()1-> client @EndC hello c |> close) in
      let (res, c) = server @EndW s in
      wait c;
      res
