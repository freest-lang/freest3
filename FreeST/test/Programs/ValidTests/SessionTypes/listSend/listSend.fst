data List = Nil | Cons Int List

flatten : List -> (rec x: 1S. +{Nil: Skip, Cons: !Int;x}) -> Skip
flatten l c =
  case l of {
    Nil -> select Nil c,
    Cons h t ->
      let c = select Cons c in
      let c = send h c in
      flatten t c
  }

reconstruct : (rec x: 1S. &{Nil: Skip, Cons: ?Int;x}) -> List
reconstruct c =
  match c with {
    Nil c -> Nil,
    Cons c ->
      let (h, c) = receive c in
      let t = reconstruct c in
      Cons h t
  }

aList, main : List

aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main =
  let (w, r) = new rec x: 1S. +{Nil: Skip, Cons: !Int;x} in
  let _ = fork @Skip $ flatten aList w in
  reconstruct r
