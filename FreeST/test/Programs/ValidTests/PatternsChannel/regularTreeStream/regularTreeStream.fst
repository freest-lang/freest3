-- Integer trees

data Tree = Leaf | Node Int Tree Tree | Error

-- Example:
--               1
--        2            6
--    8      3            7
--         5   4
aTree : Tree
aTree = Node 1 (Node 2 (Node 8 Leaf Leaf) (Node 3 (Node 5 Leaf Leaf) (Node 4 Leaf Leaf))) (Node 6 Leaf (Node 7 Leaf Leaf))

-- Integer lists and operations on Lists

data List = Nil | Cons Tree List

null : List -> Bool
null Nil        = True
null (Cons _ _) = False

head : List -> Tree
head Nil        = Leaf
head (Cons x _) = x

tail : List -> List
tail Nil         = Nil
tail (Cons _ xs) = xs

getFromSingleton : List -> Tree
getFromSingleton xs =
  if null xs
  then putStr (show @Char 'P'); Error -- "Error: Premature EndOfStream"
  else if not $ null $ tail xs
  then putStr (show @Char 'X'); Error -- "Error: Extraneous elements in the stream after reading a full tree"
  else head xs

getTwo : List -> (List, (Tree, Tree))
getTwo Nil             = putStr (show @Char 'R'); (Nil, (Error, Error)) -- "Error: Empty stack on right subtree"
getTwo (Cons left Nil) = putStr (show @Char 'L'); (Nil, (Error, left )) -- "Error: Empty stack on left subtree",
getTwo (Cons left (Cons right zs)) = (zs, (left, right))

-- Streams

type Stream = +{
    NodeC: !Int; Stream,
    LeafC: Stream,
    EndOfStream: Skip
  }

-- Writing trees on channels

streamTree : Tree -> Stream -> Stream
streamTree Leaf         c = select LeafC c
streamTree (Node x l r) c = send x $ select NodeC $ streamTree r $ streamTree l c
streamTree Error        c = select LeafC c     

sendTree : Tree -> Stream -> Skip
sendTree t c = select EndOfStream $ streamTree t c 

-- Reading trees from channels
recTree : List -> dualof Stream;a -> (Tree, a)
recTree xs (LeafC c)       = recTree @a (Cons Leaf xs) c
recTree xs (EndOfStream c) = (getFromSingleton xs, c)
recTree xs (NodeC c)       = let (xs, p) = getTwo xs in
                             let (left, right) = p in
                             let (root, c) = receive c in
                             recTree @a (Cons (Node root left right) xs) c

receiveTree : dualof Stream;a -> (Tree, a)
receiveTree c = recTree @a Nil c

-- Babdly behaving writers

writeNothing : Stream -> Skip
writeNothing c =
  select EndOfStream c

writeTooMuch : Stream -> Skip
writeTooMuch c =
  select EndOfStream $ select LeafC $ select LeafC c

writeRootTreeOnly : Stream;a -> a
writeRootTreeOnly c =
  select EndOfStream $ send 5 $ select NodeC c

writeLeftTreeOnly : Stream;a -> a
writeLeftTreeOnly c =
  select EndOfStream $ send 5 $ select NodeC $ select LeafC c

-- Go!

main : Tree
main =
  let (w, r) = new @(Stream;Close) () in
--  (fork@Skip $ sendTree aTree w);
--  (fork@Skip $ writeNothing w);       -- 'P'
--  (fork@Skip $ writeTooMuch w);     -- 'X'
  fork (\_:() 1-> writeRootTreeOnly @Close w |> close);  -- 'R'
--  (fork@Skip $ writeLeftTreeOnly w);  -- 'L'
  let (list, r) = receiveTree @Wait r in
  wait r;
  list
