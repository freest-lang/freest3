{- |
Module      :  LazyTreeTraversal
Description :  Computes the product of the values in a tree
Copyright   :  (c) Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

The example is from Thiemann and Vasconcelos: "Context-Free Session Types" (listing 4)

The server computes the product of the values in a tree;
explicitly request the values; stop as soon a zero is received.

The client sends the tree as requested by the server.

The type describing this interaction is mutually recursive. The type described by
XploreNodeChan is tail-recursive like a regular session type, but XploreTreeChan is
not since its invocations are intertwined with XploreNodeChan (adapted from the paper).

-}

data Tree = Leaf | Node Int Tree Tree

type XploreTreeChan : SL = +{Leaf: Skip,
                       Node: XploreNodeChan}

type XploreNodeChan : SL = &{
   Value : !Int;XploreNodeChan ,
   Left : XploreTreeChan ; XploreNodeChan ,
   Right : XploreTreeChan ; XploreNodeChan ,
   Exit : Skip
 }

-- The client. Send the tree as requested by the server.
exploreTree : forall a:SL => XploreTreeChan;a -> Tree -> a
exploreTree c tree =
  case tree of {
    Leaf ->
      select c Leaf,
    Node x l r ->
      exploreNode[a] (select c Node) x l r
    }

exploreNode : forall a:SL => XploreNodeChan;a -> Int -> Tree -> Tree -> a
exploreNode c x l r =
  match c with {
    Value c ->
      exploreNode[a] (send c x) x l r,
    Left c ->
      let c = exploreTree[XploreNodeChan;a] c l in
      exploreNode[a] c x l r,
    Right c ->
      let c = exploreTree[XploreNodeChan;a] c r in
      exploreNode[a] c x l r,
    Exit c ->
      c
  }

-- The server. Compute the product of the values in a tree;
-- explicitely request the values; stop as soon a zero is received
server : forall a:SL => dualof XploreTreeChan ;a -> Int -> (a, Int)
server c1 n =
  match c1 with {
    Leaf c1 ->
      (c1, n),
    Node c1 ->
      serverNode[a] c1 n
  }

serverNode : forall a:SL => dualof XploreNodeChan;a -> Int -> (a, Int)
serverNode c n =
  let (m, c) = receive (select c Value) in
  if m == 0
  then (select c Exit, 0)
  else
    let c = select c Left in
    let (c, m) = server[dualof XploreNodeChan;a] c (m * n) in
    let (c, k) = server[dualof XploreNodeChan;a] (select c Right) m in
    (select c Exit, k)

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Int
main =
  let (writer, reader) = new XploreTreeChan in
  let _ = fork (exploreTree[Skip] writer aTree) in
  let (_, n) = server[Skip] reader 1 in
  n
