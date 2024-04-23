data List = Cons Int List | Nil

type ListOut = +{Nil : Skip, Cons: !Int;ListOut}
type ListIn = dualof ListOut

rcvList : ListOut;a -> (List, a)
rcvList c =
  match c with {
    Cons c ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    Nil c -> (Nil, c)
  }

sendList : ListIn;a -> List -> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select Cons c in
      let c = send x c in
      sendList @a c xs,
    Nil       -> select Nil c
  }


main : List
main =
  let (x, y) = new @(ListOut;Close) () in
  let _      = fork @() (\_:() 1-> sendList @Close x aList |> close) in
  let (list, y) = rcvList @Wait y in
  wait y; 
  list

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
