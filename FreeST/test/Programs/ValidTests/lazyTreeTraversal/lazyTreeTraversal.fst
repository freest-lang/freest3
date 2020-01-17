data Tree = Leaf | Node Int Tree Tree

-- The client. Send the tree as requested by the server.

exploreTree : forall α:SL =>
  (rec x:SL. +{Leaf: Skip,
               Node: rec y:SL. &{Root: !Int;y,
                                 Left:  x;y,
                                 Right: x;y,
                                 Exit:  Skip}});α ->
  Tree ->
  α
exploreTree c tree =
  case tree of {
    Leaf ->
      select c Leaf,
    Node x l r ->
      exploreNode[α] (select c Node) x l r
    }

exploreNode : forall α:SL => 
  (rec y:SL. &{Root: !Int;y,
              Left:  (rec x:SL. +{Leaf: Skip,
                                  Node: rec y:SL. &{Root: !Int;y,
                                                    Left:  x;y,
                                                    Right: x;y,
                                                    Exit:  Skip}});y,
              Right: (rec x:SL. +{Leaf: Skip,
                                  Node: rec y:SL. &{Root: !Int;y,
                                                    Left:  x;y,
                                                    Right: x;y,
                                                    Exit:  Skip}});y,
              Exit:  Skip});α ->
  Int ->
  Tree ->
  Tree -> 
  α
exploreNode c x l r =
  match c with {
    Root c ->
      -- let c = send c x in
      -- exploreNode[α] c x l r,
      exploreNode[α] (send c x) x l r,
    Left c ->
      let c = exploreTree[(rec y:SL.
        &{Root: !Int;y,
          Left: +{Leaf: Skip, Node: y};y,
          Right: +{Leaf: Skip, Node: y};y,
          Exit: Skip});α] c l in
      exploreNode[α] c x l r,
    Right c ->
      let c = exploreTree[(rec y:SL.
        &{Root: !Int;y,
          Left: +{Leaf: Skip, Node: y};y,
          Right: +{Leaf: Skip, Node: y};y,
          Exit: Skip});α] c r in
      exploreNode[α] c x l r,
    Exit c ->
      c
  }

-- The server. Compute the product of the values in a tree;
-- explicitely request the values; stop as soon a zero is received
server : forall α:SL =>
  (rec x:SL. &{Leaf: Skip,
               Node: rec y:SL. +{Root: ?Int;y,
                                 Left:  x;y,
                                 Right: x;y,
                                 Exit:  Skip}});α ->
  Int ->
  (α, Int)
server c1 n =
  match c1 with {
    Leaf c1 ->
      (c1, n),
    Node c1 ->
      serverNode[α] c1 n
  }

serverNode : forall α:SL =>
  (rec y:SL. +{Root: ?Int;y,
               Left:  (rec x:SL. &{Leaf: Skip,
                                   Node: rec y:SL. +{Root: ?Int;y,
                                                     Left:  x;y,
                                                     Right: x;y,
                                                     Exit:  Skip}});y,
               Right: (rec x:SL. &{Leaf: Skip,
                                   Node: rec y:SL. +{Root: ?Int;y,
                                                     Left:  x;y,
                                                     Right: x;y,
                                                     Exit:  Skip}});y,
               Exit:  Skip});α ->
  Int ->
  (α, Int)
serverNode c n =
  let (m, c) = receive (select c Root) in
  if m == 0
  then (select c Exit, 0)
  else
    let c = select c Left in
    let (c, m) = server[(rec y:SL. +{Root: ?Int;y,
                 Left:  (rec x:SL. &{Leaf: Skip,
                                     Node: rec y:SL. +{Root: ?Int;y,
                                                       Left:  x;y,
                                                       Right: x;y,
                                                       Exit:  Skip}});y,
                 Right: (rec x:SL. &{Leaf: Skip,
                                     Node: rec y:SL. +{Root: ?Int;y,
                                                       Left:  x;y,
                                                       Right: x;y,
                                                       Exit:  Skip}});y,
                 Exit:  Skip});α] c (m * n) in
    let (c, k) = server[(rec y:SL. +{Root: ?Int;y,
                 Left:  (rec x:SL. &{Leaf: Skip,
                                     Node: rec y:SL. +{Root: ?Int;y,
                                                       Left:  x;y,
                                                       Right: x;y,
                                                       Exit:  Skip}});y,
                 Right: (rec x:SL. &{Leaf: Skip,
                                     Node: rec y:SL. +{Root: ?Int;y,
                                                       Left:  x;y,
                                                       Right: x;y,
                                                       Exit:  Skip}});y,
                 Exit:  Skip});α] (select c Right) m in
    (select c Exit, k)

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Int
main =
  let (writer, reader) = new (rec x:SL. +{Leaf: Skip, Node: rec y:SL. &{Root: !Int;y, Left: x;y, Right: x;y, Exit: Skip}}) in
  let _ = fork (exploreTree[Skip] writer aTree) in
  let (_, n) = server[Skip] reader 1 in
  n
