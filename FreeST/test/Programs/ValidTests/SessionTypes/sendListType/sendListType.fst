data List = Cons Int List | Nil

type ListOut : 1S = +{Nil : Skip, Cons: !Int;ListOut}

rcvList : forall a : 1S . dualof ListOut;a -> (List, a)
rcvList c =
  match c with {
    Cons c ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    Nil c -> (Nil, c)
  }

sendList : forall a : 1S . ListOut;a 1-> List -> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select Cons c in
      let c = send x c in
      sendList @a c xs,
    Nil       -> select Nil c
  }


main, aList : List

main =
  let (x, y) = new ListOut in
  let _      = fork @Skip \_:() 1-> sendList @Skip x aList in
  let (list, _) = rcvList @Skip y in
  list

aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
