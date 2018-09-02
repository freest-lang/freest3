{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

{-

type TreeChannel = +{
  LeafC : Skip,
  NodeC : !Int ; TreeChannel ; TreeChannel
}

type TreeChannel = rec x::Su. +{
  LeafC : Skip,
  NodeC : !Int ; x ; x
}
-}

-- data A = LeafA | NodeA Int A A
-- Type-safe serialization of a binary tree

sendTree :: forall a => Tree -> (rec x . +{LeafC : Skip, NodeC: !Int;x;x}); a -> a
sendTree t c =
  case t of
    Leaf ->
      select LeafC c
    Node x l r ->
      let w1 = select NodeC c in
      let w2 = send x w1 in
      let w3 = sendTree[(rec x . +{LeafC : Skip, NodeC: !Int;x;x});a] l w2 in
      let w4 = sendTree[a] r w3 in
      w4

receiveTree :: forall a => (rec x.&{LeafC: Skip, NodeC: ?Int;x;x}); a -> (Tree, a)
receiveTree c =
  match c with
    LeafC c1 ->
      (Leaf, c1)
    NodeC c9 ->
      let x, c2 = receive c9 in
      let left, c3 = receiveTree [(rec x.&{LeafC: Skip, NodeC: ?Int;x;x});a] c2 in
      let right, c4 = receiveTree [a] c3 in
      (Node x left right, c4)


start :: Tree
start =
  let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
  let writer,reader = new (rec x . +{LeafC: Skip, NodeC: !Int;x;x}) in
  let w = fork (sendTree[Skip] inTree writer) in
  let outTree, r = receiveTree reader in
  outTree

