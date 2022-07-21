data List = Nil | Cons Int List

flatten : List -> (rec x: 1S. +{NilC: Skip, ConsC: !Int;x}) -> Skip
flatten Nil        c = select NilC c
flatten (Cons h t) c = let c = select ConsC c in
                       let c = send h c in
                       flatten t c

reconstruct : (rec x: 1S. &{NilC: Skip, ConsC: ?Int;x}) -> List
reconstruct (NilC  c) = Nil
reconstruct (ConsC c) = let (h, c) = receive c in
                       let t = reconstruct c in
                       Cons h t

aList : List
aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main : List
main =
  let (w, r) = new rec x: 1S. +{NilC: Skip, ConsC: !Int;x} in
  let _ = fork@Skip $ flatten aList w in
  reconstruct r
