data List = Cons Int List | Nil

type ListOut : 1S = +{NilC: Skip, ConsC: !Int;ListOut}

rcvList : forall a : 1S . dualof ListOut;a -> (List, a)
rcvList c =
  match c with {
    ConsC c  ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (Cons i xs, c),
    NilC c  -> (Nil, c)
  }

sendList : forall a : 1S . ListOut;a -> List 1-> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select ConsC c in
      let c = send x c in
      sendList @a c xs,
    Nil       -> select NilC  c
  }


main, aList : List

main =
  let (x, y) = new @(ListOut;EndC) () in
  let _      = fork @() (\_:()1-> sendList @EndC x aList |> close) in
  let (list, y) = rcvList @EndW y in
  wait y;
  list

aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
