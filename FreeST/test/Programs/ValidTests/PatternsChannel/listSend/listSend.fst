data List = Nil | Cons Int List

flatten : forall a:1S . List -> (rec x: 1S. +{NilC: Skip, ConsC: !Int;x});a -> a
flatten Nil        c = select NilC c
flatten (Cons h t) c = select ConsC c
                    |> send h 
                    |> flatten @a t

reconstruct : forall a:1S . (rec x: 1S. &{NilC: Skip, ConsC: ?Int;x});a -> (List, a)
reconstruct (NilC  c) = (Nil, c)
reconstruct (ConsC c) = let (h, c) = receive c in
                       let (t, c) = reconstruct @a c in
                       (Cons h t, c)

aList : List
aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main : List
main =
  let (w, r) = new @(rec x: 1S. +{NilC: Skip, ConsC: !Int;x};Close) () in
  fork (\_:() 1-> flatten @Close aList w |> close);
  let (l, r) = reconstruct @Wait r in
  wait r;
  l
