{- |
Module      :  Exchange a binary tree on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

type TreeChannel : 1S = +{
  LeafC : Skip,
  NodeC : !Int ; TreeChannel ; TreeChannel
 }

write : forall a:1S . Tree -> TreeChannel; a -> a
write Leaf         c = select LeafC c
write (Node x l r) c =
  select NodeC c |>
  send x |>
  write @(TreeChannel;a) l |>
  write@a r

read : forall a:1S . dualof TreeChannel; a -> (Tree, a)
read (LeafC c) = (Leaf, c)
read (NodeC c) = 
  let (x, c) = receive c in
  let (left, c) = read @(dualof TreeChannel;a) c in
  let (right, c) = read @a c in
  (Node x left right, c)

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Tree
main =
  let (writer, reader) = new TreeChannel in
  fork (\_:() 1-> write @Skip aTree writer) ;
  fst @Tree@Skip $ read @Skip reader
