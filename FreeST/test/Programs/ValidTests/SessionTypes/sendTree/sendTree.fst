{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

{-
type TreeChannel = +{
  Leaf : Skip,
  Node : !Int ; TreeChannel ; TreeChannel
}

type TreeChannel = rec x::Su. +{
  Leaf : Skip,
  Node : !Int ; x ; x
}
-}

sendTree : forall a: 1S . Tree -> (rec x: 1S. +{Leaf : Skip, Node: !Int;x;x}); a -> a
sendTree t c =
  case t of {
    Leaf ->
      select Leaf c,
    Node x l r ->
      let c = select Node c in
      let c = send x c in
      let c = sendTree @((rec x: 1S. +{Leaf: Skip, Node: !Int;x;x});a) l c in
      sendTree @a r c
  }

receiveTree : forall a : 1S . (rec x: 1S. &{Leaf: Skip, Node: ?Int;x;x}); a -> (Tree, a)
receiveTree c =
  match c with {
    Leaf c ->
      (Leaf, c),
    Node c ->
      let (x, c) = receive c in
      let (left, c) = receiveTree @((rec x: 1S. &{Leaf: Skip, Node: ?Int;x;x});a) c in
      let (right, c) = receiveTree @a c in
      (Node x left right, c)
  }

main : Tree
main =
  let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
  let (writer, reader) = new (rec x: 1S . +{Leaf: Skip, Node: !Int;x;x}) in
  let w = fork @Skip (sendTree @Skip inTree writer) in
  let (outTree, r) = receiveTree @Skip reader in
  outTree
