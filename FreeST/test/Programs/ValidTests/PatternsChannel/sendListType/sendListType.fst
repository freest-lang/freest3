data List = Cons Int List | Nil

type ListOut = +{NilC : Skip, ConsC: !Int;ListOut}

rcvList : forall a . dualof ListOut;a -> (List, a)
rcvList (NilC  c) = (Nil, c)
rcvList (ConsC c) =
  let (i, c) = receive c in
  let (xs, c) = rcvList @a c in
  (Cons i xs, c)

sendList : forall a . ListOut;a -> List 1-> a
sendList c Nil         = select NilC c
sendList c (Cons x xs) =
  let c = select ConsC c in
  let c = send x c in
  sendList @a c xs

main : List
main =
  let (x, y) = new @(ListOut;End) () in
  fork (\_:() 1-> sendList @End x aList |> close) ;
  let (list, y) = rcvList @End y in
  close y;
  list

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
