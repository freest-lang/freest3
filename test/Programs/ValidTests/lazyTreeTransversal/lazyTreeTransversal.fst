data Tree = Leaf | Node Int Tree Tree

-- client

{-
type XploreTreeChan = +{
  Leaf : Skip ,
  Node : XploreNodeChan
}

rec x . +{Leaf:Skip, 
          Node: rec y . +{Value: !Int;y, 
                          Left: x;y, 
                          Right: x;y, 
                          Exit: Skip}}

type XploreNodeChan = +{
  Value : !Int ; XploreNodeChan ,
  Left : XploreTreeChan ; XploreNodeChan ,
  Right : XploreTreeChan ; XploreNodeChan ,
  Exit : Skip
}
rec y . +{Value: !Int;y, 
          Left: +{Leaf: Skip, Node: y};y, 
          Right: +{Leaf: Skip, Node: y};y, 
          Exit: Skip}
-}


exploreTree : forall α => Tree -> (
  rec x. +{LeafC: Skip,
           NodeC: rec y. &{Value: !Int;y,
                           Left:  x;y,
                           Right: x;y,
                           Exit: Skip}});α -> α
exploreTree tree c =
  case tree of {
    Leaf ->
      select LeafC c;
    Node x l r ->
      let c = select NodeC c in
      exploreNode[α] x l r c
    }

exploreNode : forall α => Int -> Tree -> Tree -> (
  rec y. &{Value: !Int;y,
           Left:  +{LeafC: Skip, NodeC: y};y,
           Right: +{LeafC: Skip, NodeC: y};y,
           Exit: Skip});α -> α
exploreNode x l r c =
  match c with {
    Value c ->
      let c = send c x in
      (exploreNode[α] x l r c);
    Left c ->
      let c = exploreTree[(rec y. &{Value: !Int;y, Left: +{LeafC: Skip, NodeC: y};y, Right: +{LeafC: Skip, NodeC: y};y, Exit: Skip});α] l c in
      (exploreNode[α] x l r c);
    Right c ->
      let c = exploreTree[(rec y. &{Value: !Int;y, Left: +{LeafC: Skip, NodeC: y};y, Right: +{LeafC: Skip, NodeC: y};y, Exit: Skip});α] r c in
      (exploreNode[α] x l r c);
    Exit c1 ->
      c1
  }
-- server: compute the product of the values in a tree; stop as soon a zero is received
-- to be completed

server : forall α => Int -> (
  rec x. &{LeafC: Skip,
           NodeC: rec y. +{Value: ?Int;y,
                           Left:  x;y,
                           Right: x;y,
                           Exit:  Skip}});α -> (Int, α)
server n c =
  match c with {
    LeafC c ->
      (n, c);
    NodeC c ->
      serverNode[α] n c
  }

serverNode : forall α => Int -> (
  rec y. +{Value: ?Int;y,
           Left:  &{LeafC: Skip, NodeC: y};y,
           Right: &{LeafC: Skip, NodeC: y};y,
           Exit:  Skip});α -> (Int, α)
serverNode n c =
  let c = select Value c in
  let m, c = receive c in
  if m == 0
  then let c = select Exit c in (0, c)
  else
    let c = select Left c in
    let c, m = server[α] (m * n) c in
    let c = select Right c in
    server[α] m c

-- main

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Int
main =
  let writer, reader = new (rec x. +{LeafC: Skip, NodeC: rec y. &{Value: !Int;y, Left: x;y, Right: x;y, Exit: Skip}}) in
  let x = fork (exploreTree[Skip] aTree writer) in
  let n, r = server[Skip] 1 reader in
  n

