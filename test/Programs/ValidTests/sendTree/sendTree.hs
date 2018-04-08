{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}
{-
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

-- Type-safe serialization of a binary tree
--sendTree :: a :: SU => Tree -> rec x::SU.+{Leaf: Skip, Node: !Int;x;x} ; a -> a
sendTree :: Tree -> rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a -> a
sendTree t c =
  case t of
    Leaf ->
      select LeafC c
    Node x l r ->
      let c1 = select NodeC c in
      let c2 = send x c1 in
      let c3 = sendTree[rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a] l c2 in
      let c4 = sendTree[a] r c3 in
      c4

receiveTree :: rec x.&{LeafC: Skip, NodeC: ?Int;x;x} ; a -> (Tree, a)
receiveTree c =
  case c of
    LeafC c1 ->
      (Leaf, c1)
    NodeC c1 ->
      let x, c2 = receive c1 in
      let left, c3 = receiveTree[rec x.&{LeafC: Skip, NodeC: ?Int;x;x} ; a] c2 in
      let right, c4 = receiveTree[a] c3 in
      (Node x left right, c4)

start :: Tree
start =
 let inTree = Node 7 (Node 5 (Leaf) (Leaf)) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
 let writer,reader = new rec x.+{LeafC: Skip, NodeC: !Int;x;x} in
 let w = fork (sendTree[Skip] inTree writer) in
 let outTree, r = receiveTree[Skip] reader in
 outTree

{-
TODO:
try the same constructors for the datatype and the session type

        rec x.+{Leaf: Skip, Node: !Int;x;x}
-}
-}
start :: Int
start = 10
