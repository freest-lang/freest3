data List = Cons Int List | Nil

type ListOut : 1S = +{NilC: Skip, ConsC: !Int;ListOut}
type ListIn  : 1S = dualof ListOut

rcvList : forall a : 1S . ListIn;a -> (List, a)
rcvList (NilC  c) = (Nil, c)
rcvList (ConsC c) =
  let (i, c) = receive c in
  let (xs, c) = rcvList@a c in
  (Cons i xs, c)

sendList : forall a : 1S . ListOut;a -> List 1-> a
sendList c Nil = select NilC c
sendList c (Cons x xs) =
  let c = select ConsC c in
  let c = send x c in
  sendList@a c xs


main : List
main =
  let (x, y) = new ListOut in
  fork (\_:() 1-> sendList@Skip x aList) ;
  let (list, _) = rcvList@Skip y in
  list

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
