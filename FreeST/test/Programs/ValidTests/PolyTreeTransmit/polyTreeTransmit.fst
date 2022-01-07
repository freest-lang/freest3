------------------------------------------------------------
-- Send a poly tree
------------------------------------------------------------

data Tree a = Node a (Tree a) (Tree a) | Leaf
type TreeChannel:SL a:MU = +{ Node: !a;(TreeChannel a);(TreeChannel a), Leaf: Skip }

sendTree : ∀a:MU b:SL . Tree a → (TreeChannel a);b → b
sendTree t c =
  case t of {
    Leaf -> select Leaf c,
    Node x l r ->
      let c = select Node c in
      let c = send x c in
      let c = sendTree[a, (TreeChannel a);b] l c in
      sendTree[a, b] r c
  }

rcvTree : ∀ a:MU b:SL . dualof (TreeChannel a); b -> (Tree a, b)
rcvTree c =
  match c with {
    Leaf c ->
      (Leaf[a], c),
    Node c ->  
      let (x, c) = receive c in
      let (left, c) = rcvTree [a, dualof (TreeChannel a); b] c in 
      let (right, c) = rcvTree [a, b] c in
      (Node[a] x left right, c)
  }
  
aTree : Tree Int
aTree = Node[Int] 7 (Node[Int] 5 (Leaf[Int]) (Leaf[Int]))
   (Node[Int] 9 (Node[Int] 11 (Leaf[Int]) (Leaf[Int])) (Node[Int] 15 (Leaf[Int]) (Leaf[Int])))

main : Tree Int
main =
  let (writer, reader) = new TreeChannel Int in
  fork $ sendTree [Int, Skip] aTree writer;
  fst [Tree Int, Skip] $ rcvTree [Int, Skip] reader
