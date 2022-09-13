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
null xs = case xs of {
    Nil -> True,
    Cons _ _ -> False
  }

head : List -> Tree
head xs =
  case xs of {
    Nil -> Leaf,
    Cons x _ -> x
  }

tail : List -> List
tail xs =
  case xs of {
    Nil -> Nil,
    Cons _ xs -> xs
  }

getFromSingleton : List -> Tree
getFromSingleton xs =
  if null xs
  then printChar 'P'; Error -- "Error: Premature EndOfStream"
  else if not $ null $ tail xs
  then printChar 'X'; Error -- "Error: Extraneous elements in the stream after reading a full tree"
  else head xs

getTwo : List -> (List, (Tree, Tree))
getTwo xs  =
  case xs of {
    Nil -> printChar 'R'; (Nil, (Error, Error)), -- "Error: Empty stack on right subtree"
    Cons left ys -> case ys of {
      Nil -> printChar 'L'; (Nil, (Error, left)), -- "Error: Empty stack on left subtree",
      Cons right zs -> (zs, (left, right))
    }
  }

-- Streams

type Stream : 1S = +{
    Node: !Int; Stream,
    Leaf: Stream,
    EndOfStream: End
  }

-- Writing trees on channels

sendTree : Tree -> Stream -> ()
sendTree t c = streamTree t c |> select EndOfStream |> close

streamTree : Tree -> Stream -> Stream
streamTree t c =
  case t of {
    Leaf ->
      select Leaf c,
    Node x l r ->
      send x $ select Node $ streamTree r $ streamTree l c,
    Error ->
      select Leaf c
  }

-- Reading trees from channels

receiveTree : dualof Stream -> Tree
receiveTree = recTree Nil

recTree : List -> dualof Stream -> Tree
recTree xs c =
  match c with {
    Node c ->
      let (xs, p) = getTwo xs in
      let (left, right) = p in
      let (root, c) = receive c in
      recTree (Cons (Node root left right) xs) c,
    Leaf c ->
      recTree (Cons Leaf xs) c,
    EndOfStream c -> close c; getFromSingleton xs
  }

-- Babdly behaving writers

writeNothing, writeTooMuch, writeRootTreeOnly, writeLeftTreeOnly : Stream -> ()
writeNothing c =
  select EndOfStream c |> close

writeTooMuch c =
  select Leaf c |> select Leaf |> select EndOfStream |> close

writeRootTreeOnly c =
  select Node c |> send 5 |> select EndOfStream |> close

writeLeftTreeOnly c =
  select Leaf c |> select Node |> send 5 |> select EndOfStream |> close

-- Go!

main : Tree
main =
  let (w, r) = new Stream;End in
--  (fork[Skip] $ sendTree aTree w);
--  (fork[Skip] $ writeNothing w);       -- 'P'
--  (fork[Skip] $ writeTooMuch w);     -- 'X'
  fork @() (\_:()1-> writeRootTreeOnly w);  -- 'R'
--  (fork[Skip] $ writeLeftTreeOnly w);  -- 'L'
  receiveTree r
