data List = Cons Int List | Nil

type ListOut = +{NilC: Skip, ConsC: !Int;ListOut}
type ListIn  = dualof ListOut

rcvList : ListIn;a -> (List, a)
rcvList (NilC  c) = (Nil, c)
rcvList (ConsC c) =
  let (i, c) = receive c in
  let (xs, c) = rcvList@a c in
  (Cons i xs, c)

sendList : ListOut;a -> List 1-> a
sendList c Nil = select NilC c
sendList c (Cons x xs) =
  let c = select ConsC c in
  let c = send x c in
  sendList@a c xs

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))

main : List
main =
  let (x, y) = new @(ListOut;Close) () in
  fork (\_:() 1-> sendList@Close x aList |> close) ;
  let (list, y) = rcvList@Wait y in
  wait y;
  list

