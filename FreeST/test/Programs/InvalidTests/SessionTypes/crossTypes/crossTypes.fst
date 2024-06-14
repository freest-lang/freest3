data List = Cons Int List | Nil

type ListOut = +{Nil : Skip, Cons: !Int;ListOut}

rcvList : dualof ListOut;a -> (List, a)
rcvList c =
  match c with {
    Cons c ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    Nil c -> (Nil, c)
  }

sendList : ListOut;a -> List -> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select Cons c in
      let c = send x c in
      sendList @a c xs,
    Nil       -> select Nil c
  }

aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))

main : List
main =
  let (x, y) = new @(ListOut;Close) () in
  fork @() (\_:() 1-> sendList @Close y aList |> close);
  let (list, x) = rcvList @Wait x in
  wait x;
  list

