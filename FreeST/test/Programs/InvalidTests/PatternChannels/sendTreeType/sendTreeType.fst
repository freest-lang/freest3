{- |
Module      :  Exchange a binary tree on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

type TreeChannel : SL = +{
  Leaf : Skip,
  Node : !Int ; TreeChannel ; TreeChannel
 }

write : forall a:SL . Tree -> TreeChannel; a -> a
write Leaf         c = select Leaf c
write (Node x l r) c =
  select Node c &
  send x &
  write [TreeChannel;a] l &
  write[a] r

read : forall a:SL . dualof TreeChannel; a -> (Tree, a)
read (Leaf c) = (Leaf, c)
read (Node c) = 
  let (x, c) = receive c in
  let (left, c) = read [dualof TreeChannel;a] c in
  let (right, c) = read [a] c in
  (Node x left right, c)

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Tree
main =
  let (writer, reader) = new TreeChannel in
  fork [Skip] $ write [Skip] aTree writer;
  fst [Tree, Skip] $ read [Skip] reader
