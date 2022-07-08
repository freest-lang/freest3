data Tree = Leaf | Node Int Tree Tree

-- Note: we use the same constructors for the datatype and the channel, namely Leaf and Node

{-
  Writes a tree on a given channel;
  for each node in the tree reads an integer from the channel;
  returns a tree isomorphic to the input where each integer in nodes
  is read from the channel.
-}
transformTree : Tree -> (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) -> (Tree, Skip)
transformTree tree c =
  case tree of {
    Leaf ->
      (Leaf, select c LeafC),
    Node x l r ->
      let c = select c NodeC in
      let c = send c x in
      let (l, c) = transformNode l c in
      let (r, c) = transformTreeInt r c in
      let (y, c) = receive c in
      (Node y l r, c)
  }

transformNode : Tree ->
              (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) ;
              (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) ;
              ?Int ->
              (Tree, (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) ; ?Int)
transformNode tree c =
  case tree of {
    Leaf ->
      (Leaf, select c LeafC),
    Node x l r ->
      let c = select c NodeC in
      let c = send c x in
      let (l, c) = transformNode l c in
      let (r, c) = transformTreeInt r c in
      let (y, c) = receive c in
      (Node y l r, c)
  }

transformTreeInt : Tree ->
              (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) ;
              ?Int ->
              (Tree, (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}))
transformTreeInt tree c =
  case tree of {
    Leaf ->
      (Leaf, select c LeafC),
    Node x l r ->
      let c = select c NodeC in
      let c = send c x in
      let (l, c) = transformNode l c in
      let (r, c) = transformTreeInt r c in
      let (y, c) = receive c in
      (Node y l r, c)
  }

{-
  Reads a tree from a given channel;
  writes back on the channel the sum of the elements in the tree;
  returns this sum.
-}
treeSum : forall α : 1S => (rec x: 1S. &{LeafC: Skip, NodeC: ?Int;x;x;!Int});α -> (Int, α)
treeSum c =
  match c with {
    LeafC c -> (0, c),
    NodeC c ->
      let (x, c) = receive c in
      let (l, c) = treeSum @((rec x: 1S. &{LeafC: Skip, NodeC: ?Int;x;x;!Int});!Int;α) c in
      let (r, c) = treeSum @(!Int) @α c in
      let c    = send c (x + l + r) in
      (x + l + r, c)
  }

aTree : Tree
aTree = Node 1 (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf (Node 5 Leaf Leaf)))) (Node 6 Leaf (Node 7 Leaf (Node 8 Leaf Leaf)))

main : Tree
main =
  let (w, r) = new (rec x: 1S. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) in
--  let t, w = fork (transform[Skip] aTree w) in
--  let n, r = treeSum[Skip] r in
  let _ = fork (treeSum @Skip r) in
  let (t, _) = transformTree aTree w in
  t
