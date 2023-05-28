{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

type TreeChannel : 1S = +{ LeafC: Skip
                         , NodeC: !Int; TreeChannel; TreeChannel
                         }

sendTree : forall a:1S . Tree -> TreeChannel; a -> a
sendTree Leaf         c = select LeafC c
sendTree (Node x l r) c =
  let c = select NodeC c in
  let c = send x c in
  let c = sendTree@(TreeChannel;a) l c in
  sendTree@a r c

receiveTree : forall a : 1S . dualof TreeChannel; a -> (Tree, a)
receiveTree (LeafC c) = (Leaf, c)
receiveTree (NodeC c) = 
  let (x, c) = receive c in
  let (left, c) = receiveTree @(dualof TreeChannel;a) c in
  let (right, c) = receiveTree@a c in
  (Node x left right, c)

main : Tree
main =
  let inTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
  let (writer, reader) = new @(TreeChannel;EndC) () in
  fork (\_:() 1-> sendTree @EndC inTree writer |> close) ;
  let (outTree, r) = receiveTree @EndW reader in
  wait r;
  outTree
