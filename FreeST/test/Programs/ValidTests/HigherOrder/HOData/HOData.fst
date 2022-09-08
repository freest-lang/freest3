data Tree = Leaf | Node Tree Int Tree

singleton : Int -> Tree
singleton x = Node Leaf x Leaf

aTree : Tree
aTree =
  Node (
    Node 
      (singleton 4)
      6
      (singleton 8))
    0
    (singleton 5)

main : Tree
main =
  let (w, r) = new !Tree;End in
  fork (send aTree w & close);
  let (t, r) = receive r in 
  close r; 
  t
