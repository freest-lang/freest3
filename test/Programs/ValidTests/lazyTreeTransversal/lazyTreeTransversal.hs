 -- rec xPloreTreeChan . +{Leaf: Skip, Node: xPloreTreeChan}
 -- rec xPloreNodeChan . +{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip}

data Tree = Leaf | Node Int Tree Tree


exploreTree :: forall x => Tree -> (rec xPloreTreeChan . +{LeafC: Skip, NodeC: xPloreTreeChan});x -> x
exploreTree tree c =
  case tree of
    Leaf ->
      select LeafC c
    Node x l r ->
      let c1 = select NodeC c in
      exploreNode[rec xPloreNodeChan . +{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip}] x l r c1


exploreNode :: forall x => Int -> Tree -> Tree -> (rec xPloreNodeChan . &{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip});x -> x
exploreNode x l r c1 =
  match c1 with
    Value c2 ->
      let c3 = send x c2 in
      (exploreNode[rec xPloreNodeChan . &{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip}] x l r c3)
    Left c2 ->
      let c3 = exploreTree[rec xPloreTreeChan . +{LeafC: Skip, NodeC: xPloreTreeChan}] l c2 in
      (exploreNode[rec xPloreNodeChan . &{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip}] x l r c3)
    Right c2 ->
      let c3 = exploreTree[rec xPloreTreeChan . +{LeafC: Skip, NodeC: xPloreTreeChan}] r c2 in
      (exploreNode[rec xPloreNodeChan . &{Value: !Int;xPloreNodeChan, Left: xPloreNodeChan;xPloreNodeChan, Right: xPloreNodeChan;xPloreNodeChan, Exit: Skip}] x l r c3)
    Exit c2 ->
      c2


start :: ()
start =
  let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
  let writer, reader = new (rec xPloreTreeChan . +{LeafC: Skip, NodeC: xPloreTreeChan}) in
  let w = fork(exploreTree[Skip] inTree writer) in
  ()
-- let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
--   let writer,reader = new (rec x . +{LeafC: Skip, NodeC: !Int;x;x}) in
--   let w = fork (sendTree[Skip] inTree writer) in
--   let outTree, r = receiveTree reader in
--   outTree
