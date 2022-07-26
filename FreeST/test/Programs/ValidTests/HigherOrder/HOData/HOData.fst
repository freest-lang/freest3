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

main : (Tree, Skip)
main =
  let (w, r) = new !Tree in
  fork (\_:() 1-> send aTree w);
  receive r
