data List = Nil | Cons Int List

flatten : List -> (rec x: SL. +{Nil: Skip, Cons: !Int;x}) -> Skip
flatten Nil c        = select Nil c
flatten (Cons h t) c = let c = select Cons c in
                       let c = send h c in
                       flatten t c

reconstruct : (rec x: SL. &{Nil: Skip, Cons: ?Int;x}) -> List
reconstruct (Nil  c) = Nil
reconstruct (Cons c) = let (h, c) = receive c in
                       let t = reconstruct c in
                       Cons h t

aList : List
aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main : List
main =
  let (w, r) = new rec x: SL. +{Nil: Skip, Cons: !Int;x} in
  let _ = fork[Skip] $ flatten aList w in
  reconstruct r
