data Tree = Leaf | Node Int Tree Tree

exploreTree : forall α:SL =>
  (rec x:SL. +{Leaf: Skip,
               Node: rec y:SL. &{Value: !Int;y,
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
  (rec y:SL. &{Value: !Int;y,
              Left:  (rec x:SL. +{Leaf: Skip,
                                  Node: rec y:SL. &{Value: !Int;y,
                                                    Left:  x;y,
                                                    Right: x;y,
                                                    Exit:  Skip}});y,
              Right: (rec x:SL. +{Leaf: Skip,
                                  Node: rec y:SL. &{Value: !Int;y,
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
    Value c ->
      let c = send c x in
      exploreNode[α] c x l r,
    Left c ->
      let c = exploreTree[(rec y:SL. &{Value: !Int;y, Left: +{Leaf: Skip, Node: y};y, Right: +{Leaf: Skip, Node: y};y, Exit: Skip});α] c l in
      exploreNode[α] c x l r,
    Right c ->
      let c = exploreTree[(rec y:SL. &{Value: !Int;y, Left: +{Leaf: Skip, Node: y};y, Right: +{Leaf: Skip, Node: y};y, Exit: Skip});α] c r in
      exploreNode[α] c x l r,
    Exit c ->
      c
  }

-- server: compute the product of the values in a tree; stop as soon a zero is received
-- to be completed
server : forall α:SL =>
  (rec x:SL. &{Leaf: Skip,
               Node: rec y:SL. +{Value: ?Int;y,
                                 Left:  x;y,
                                 Right: x;y,
                                 Exit:  Skip}});α ->
  Int ->
  (α, Int)
server c n =
  match c with {
    Leaf c ->
      (c, n),
    Node c ->
      serverNode[α] c n
  }

serverNode : forall α:SL =>
  (rec y:SL. +{Value: ?Int;y,
               Left:  (rec x:SL. &{Leaf: Skip,
                                   Node: rec y:SL. +{Value: ?Int;y,
                                                     Left:  x;y,
                                                     Right: x;y,
                                                     Exit:  Skip}});y,
               Right: (rec x:SL. &{Leaf: Skip,
                                   Node: rec y:SL. +{Value: ?Int;y,
                                                     Left:  x;y,
                                                     Right: x;y,
                                                     Exit:  Skip}});y,
               Exit:  Skip});α ->
  Int ->
  (α, Int)
serverNode c n =
  let c = select c Value in
  let m, c = receive c in
  if m == 0
  then (select c Exit, 0)
  else
    let c = select c Left in
    let c, m = server[α] c (m * n) in
    server[α] (select c Right) m

{-
-- main

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : Int
main =
  let writer, reader = new (rec x:SL. +{Leaf: Skip, Node: rec y:SL. &{Value: !Int;y, Left: x;y, Right: x;y, Exit: Skip}}) in
  let x = fork (exploreTree[Skip] aTree writer) in
  let n, r = server[Skip] 1 reader in
  n
-}

main : Int
main = 5
