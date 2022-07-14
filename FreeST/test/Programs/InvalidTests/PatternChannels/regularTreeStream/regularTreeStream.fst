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
  then printChar 'P'; Error -- "Error: Premature EndOfStream"
  else if not $ null $ tail xs
  then printChar 'X'; Error -- "Error: Extraneous elements in the stream after reading a full tree"
  else head xs

getTwo : List -> (List, (Tree, Tree))
getTwo Nil             = printChar 'R'; (Nil, (Error, Error)) -- "Error: Empty stack on right subtree"
getTwo (Cons left Nil) = printChar 'L'; (Nil, (Error, left )) -- "Error: Empty stack on left subtree",
getTwo (Cons left (Cons right zs)) = (zs, (left, right))

-- Streams

type Stream : 1S = +{
    NodeC: !Int; Stream,
    LeafC: Stream,
    EndOfStream: Skip
  }

-- Writing trees on channels

sendTree : Tree -> Stream -> Skip
sendTree t c = select EndOfStream $ streamTree t c

streamTree : Tree -> Stream -> Stream
streamTree Leaf c         = select LeafC c
streamTree (Node x l r) c = send x $ select Node $ streamTree r $ streamTree l c
streamTree Error         = select LeafC c      

-- Reading trees from channels

receiveTree : dualof Stream -> Tree
receiveTree = recTree Nil

recTree : List -> dualof Stream -> Tree
recTree xs (LeafC c)       = recTree (Cons Leaf xs) c
recTree xs (EndOfStream _) = getFromSingleton xs
recTree xs (NodeC c)       = let (xs, p) = getTwo xs in
                             let (left, right) = p in
                             let (root, c) = receive c in
                             recTree (Cons (Node root left right) xs) c

-- Babdly behaving writers

writeNothing : Stream -> Skip
writeNothing c =
  select EndOfStream c

writeTooMuch : Stream -> Skip
writeTooMuch c =
  select EndOfStream $ select Leaf $ select LeafC c

writeRootTreeOnly : Stream -> Skip
writeRootTreeOnly c =
  select EndOfStream $ send 5 $ select NodeC c

writeLeftTreeOnly : Stream -> Skip
writeLeftTreeOnly c =
  select EndOfStream $ send 5 $ select Node $ select LeafC c

-- Go!

main : Tree
main =
  let (w, r) = new Stream in
--  (fork@Skip $ sendTree aTree w);
--  (fork@Skip $ writeNothing w);       -- 'P'
--  (fork@Skip $ writeTooMuch w);     -- 'X'
  (fork@Skip $ writeRootTreeOnly w);  -- 'R'
--  (fork@Skip $ writeLeftTreeOnly w);  -- 'L'
  receiveTree r
