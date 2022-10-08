{- |
Module      : Exchange a binary tree on a channel
Description : Uses regular, higher-order, channels,
              rather than first-order, context-free
Copyright   : (c) LASIGE and University of Lisbon, Portugal
Maintainer  : vmavsconcelos@ciencias.ulisboa.pt
-}

data Tree = Leaf | Node Tree Int Tree

aTree : Tree
aTree = Node (Node Leaf 5 Leaf) 7 (Node (Node Leaf 11 Leaf) 9 (Node Leaf 15 Leaf))

type TreeC : 1S = &{
  LeafC: End,
  NodeC: ?TreeC ; ?Int ; ?TreeC ; End
 }

read : TreeC -> Tree
read (LeafC c) = close c ; Leaf
read (NodeC c) =
  let (l, c) = receive c in
  let (x, c) = receive c in
  let (r, c) = receive c in
  close c ;
  Node (read l) x (read r)

write : Tree -> dualof TreeC -> ()
write Leaf c = c |> select LeafC |> close
write (Node l x r) c =
  c |> select NodeC
    |> send (forkWith @TreeC @() (write l))
    |> send x
    |> send (forkWith @TreeC @() (write r))
    |> close

main : Tree
main =
  forkWith @TreeC @() (write aTree) |>
  read
