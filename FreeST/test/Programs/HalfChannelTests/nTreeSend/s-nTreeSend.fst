-- Represents a n-Tree structure where each node has 0..n children.
data Tree = Empty | Node Int TreeList

-- List of Trees
data TreeList = Nil | Cons Tree TreeList

type TreeChannel = &{
  Node : ?Int; TreeListChannel,
  Empty: Skip }

type TreeListChannel = &{
  Cons: TreeChannel; TreeListChannel,
  Nil : Skip }

-- ===== RECEIVING =====

receiveTree : TreeChannel;a -> (Tree, a)
receiveTree c =
  match c with {
    Empty c ->
      (Empty, c),
    Node c ->
      let (i, c)        = receive c in
      let (children, c) = receiveTreeList @a c in
      (Node i children, c)
  }

and receiveTreeList : TreeListChannel;a -> (TreeList, a)
receiveTreeList c =
  match c with {
    Nil c ->
      (Nil, c),
    Cons c ->
      let (tree, c) = receiveTree @(TreeListChannel ; a) c in
      let (rest, c) = receiveTreeList @a c in
      (Cons tree rest, c)
  }

main : Tree
main =
  let (t, c) = newHcServer @(TreeChannel;Wait) ("127.0.0.1", "8081") |>
  receiveTree @Wait in
  wait c;
  t
