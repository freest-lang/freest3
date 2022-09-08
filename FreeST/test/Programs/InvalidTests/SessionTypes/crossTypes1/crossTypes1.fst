data List = Cons Int List | Nil

type ListOut : 1S = +{Nil : Skip, Cons: !Int;ListOut}
type ListIn = dualof ListOut

rcvList : forall a : 1S . ListOut;a -> (List, a)
rcvList c =
  match c with {
    Cons c ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    Nil c -> (Nil, c)
  }

sendList : forall a : 1S . ListIn;a -> List -> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select c Cons in
      let c = send c x in
      sendList @a c xs,
    Nil       -> select c Nil
  }


main : List
main =
  let (x, y) = new ListOut;End in
  let _      = fork @() (\_:()-> sendList @End x aList & close) in
  let (list, y) = rcvList @End y in
  close y; 
  list

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
