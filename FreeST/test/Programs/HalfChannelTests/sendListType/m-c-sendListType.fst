data List = Cons Int List | Nil

type ListOut = +{NilC: Skip, ConsC: !Int;ListOut}

sendList : ListOut;a -> List 1-> a
sendList c l =
  case l of {
    Cons x xs ->
      let c = select ConsC c in
      let c = send x c in
      sendList @a c xs,
    Nil       -> select NilC  c
  }


aList : List
aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))

main : ()
main =
  let c = newHcClient @(ListOut;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") in
  sendList @Close c aList |> close
