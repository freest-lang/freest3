data List = Nil | Cons Int List

type SendList : 1S = +{Nil: Skip, Cons: !Int;SendList}
type RecvList : 1S = dualof SendList

flatten : ∀ a:1S . List -> SendList;a -> a
flatten l c =
  case l of {
    Nil -> select Nil c,
    Cons h t ->
      let c = select Cons c in
      let c = send h c in
      flatten @a t c
  }

reconstruct : ∀ a:1S . RecvList;a -> (List, a)
reconstruct c =
  match c with {
    Nil c -> (Nil, c),
    Cons c ->
      let (h, c) = receive c in
      let (t, c) = reconstruct @a c in
      (Cons h t, c)
  }

aList, main : List

aList = Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil))))

main =
  let (w, r) = new @(SendList;End) () in
  fork @() (\_:()1-> flatten @End aList w |> close);
  let (l, c) = reconstruct @End r in 
  close c;
  l
