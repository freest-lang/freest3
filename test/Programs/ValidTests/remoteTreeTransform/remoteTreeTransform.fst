data Tree = Leaf | Node Int Tree Tree

-- Note: we use the same constructors for the datatype and the channel, namely Leaf and Node

{-
  Writes a tree on a given channel;
  for each node in the tree reads an integer from the channel;
  returns a tree isomorphic to the input where each integer in nodes
  is read from the channel.
-}
transform : forall α => Tree -> (rec x. +{LeafC: Skip, NodeC: !Int;x;x;?Int});α -> (Tree, α)
transform tree c =
  case tree of
    Leaf ->
      (Leaf, select LeafC c);
    Node x l r ->
      let c = select NodeC c in
      let c = send x c in
      let l, c = transform[(rec x. +{LeafC: Skip, NodeC: !Int;x;x;?Int});?Int;α] l c in
      let r, c = transform[?Int;α] r c in
      let y, c = receive c in
      (Node y l r, c)

{-
  Reads a tree from a given channel;
  writes back on the channel the sum of the elements in the tree;
  returns this sum.
-}
treeSum : forall α => (rec x. &{LeafC: Skip, NodeC: ?Int;x;x;!Int});α -> (Int, α)
treeSum c =
  match c with
    LeafC c -> (0, c);
    NodeC c ->
      let x, c = receive c in
      let l, c = treeSum[(rec x. &{LeafC: Skip, NodeC: ?Int;x;x;!Int});!Int;α] c in
      let r, c = treeSum[!Int;α] c in
      let c    = send (x + l + r) c in
      (x + l + r, c)

aTree : Tree
aTree = Node 1 (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf (Node 5 Leaf Leaf)))) (Node 6 Leaf (Node 7 Leaf (Node 8 Leaf Leaf)))

main : Tree
main =
  let w, r = new (rec x. +{LeafC: Skip, NodeC: !Int;x;x;?Int}) in
--  let t, w = fork (transform[Skip] aTree w) in
--  let n, r = treeSum[Skip] r in
  let _ = fork (treeSum[Skip] r) in
  let t, _ = transform[Skip] aTree w in
  t
