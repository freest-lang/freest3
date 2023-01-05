{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

{-
type TreeChannel = +{
  LeafC: Skip,
  NodeC: !Int ; TreeChannel ; TreeChannel
}

type TreeChannel = rec x::Su. +{
  LeafC: Skip,
  NodeC: !Int ; x ; x
}
-}

sendTree : forall a:1S . Tree -> (rec x:1S. +{LeafC: Skip, NodeC: !Int;x;x}); a -> a
sendTree Leaf         c = select LeafC c
sendTree (Node x l r) c =
  let c = select NodeC c in
  let c = send x c in
  let c = sendTree@((rec x:1S. +{LeafC: Skip, NodeC: !Int;x;x});a) l c in
  sendTree@a r c

receiveTree : forall a : 1S . (rec x:1S. &{LeafC: Skip, NodeC: ?Int;x;x}); a -> (Tree, a)
receiveTree (LeafC c) = (Leaf, c)
receiveTree (NodeC c) = 
  let (x, c) = receive c in
  let (left, c) = receiveTree @((rec x:1S. &{LeafC: Skip, NodeC: ?Int;x;x});a) c in
  let (right, c) = receiveTree@a c in
  (Node x left right, c)

main : Tree
main =
  let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
  let (writer, reader) = new @(rec x:1S . +{LeafC: Skip, NodeC: !Int;x;x}) () in
  fork (\_:() 1-> sendTree@Skip inTree writer) ;
  let (outTree, r) = receiveTree@Skip reader in
  outTree
