data Tree = Leaf | Node () Tree Tree

type TreeChannel = +p{LeafC: Skip, NodeC: !p+1(); TreeChannel; TreeChannel}

sendTree : Tree ->[top,bot] TreeChannel; a ->[p,p+1] a
sendTree Leaf         c = select LeafC c
sendTree (Node x l r) c =
  let c = select NodeC c in
  let c = send x c in
  let c = sendTree@(TreeChannel;a) l c in
  sendTree@a r c

receiveTree : dualof TreeChannel; a ->[p,p+1] (Tree, a)
receiveTree (LeafC c) = (Leaf, c)
receiveTree (NodeC c) = 
  let (x, c) = receive c in
  let (left, c) = receiveTree @(dualof TreeChannel;a) c in
  let (right, c) = receiveTree@a c in
  (Node x left right, c)

main : Tree
main =
  let inTree = Node () (Node () Leaf Leaf) (Node () (Node () Leaf Leaf) (Node () Leaf Leaf)) in
  let (writer, reader) = new @(TreeChannel; Close p+2) () in
  fork (\_:() 1-> sendTree @Close p+2 inTree writer |> close) ;
  let (outTree, r) = receiveTree @Wait p+2 reader in
  wait r;
  outTree