data List = Nil | Cons Int List

flatten : List -> rec x: SL. +{Nil: Skip, Cons: !Int;x} -> Skip
flatten l c =
  case l of {
    Nil -> select c Nil,
    Cons h t ->
      let c = select c Cons in
      let c = send c h in
      flatten t c
  }

reconstruct : rec x: SL. &{Nil: Skip, Cons: ?Int;x} -> List
reconstruct c =
  match c with {
    Nil c -> Nil,
    Cons c ->
      let (h, c) = receive c in
      let t = reconstruct c in
      Cons h t
  }

aList : List
aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main : List
main =
  let (w, r) = new rec x: SL. +{Nil: Skip, Cons: !Int;x} in
  let _ = fork (flatten aList w) in
  reconstruct r

