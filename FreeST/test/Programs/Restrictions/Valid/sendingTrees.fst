data Tree = Leaf | Node () Tree Tree

type TreeChannel = +1{LeafC: Skip, NodeC: !2(); TreeChannel; TreeChannel}

sendTree : Tree ->[top,bot] TreeChannel; a ->[1,2] a
sendTree Leaf         c = select LeafC c
sendTree (Node x l r) c =
  let c = select NodeC c in
  let c = send x c in
  let c = sendTree@(TreeChannel;a) l c in
  sendTree@a r c

receiveTree : dualof TreeChannel; a ->[1,2] (Tree, a)
receiveTree (LeafC c) = (Leaf, c)
receiveTree (NodeC c) = 
  let (x, c) = receive c in
  let (left, c) = receiveTree @(dualof TreeChannel;a) c in
  let (right, c) = receiveTree@a c in
  (Node x left right, c)

main : Tree
main =
  let inTree = Node () (Node () Leaf Leaf) (Node () (Node () Leaf Leaf) (Node () Leaf Leaf)) in
  let (writer, reader) = new @(TreeChannel; Close 3) () in
  fork (\_:() 1-> sendTree @Close 3 inTree writer |> close) ;
  let (outTree, r) = receiveTree @Wait 3 reader in
  wait r;
  outTree