-- Integer trees

data Tree = Leaf | Node Int Tree Tree | Error

-- Tree lists and operations on Lists

data List = Nil | Cons Tree List

null : List -> Bool
null Nil = True
null (Cons _ _) = False

head : List -> Tree
head (Cons x _) = x

tail : List -> List
tail (Cons _ xs) = xs

getFromSingleton : List -> Tree
getFromSingleton (Cons x Nil) = x
getFromSingleton Nil = print @String "Error: Premature EndOfStream" ; Error
getFromSingleton _ = print @String "Error: Extraneous elements in the stream after reading a full tree" ; Error

getTwo : List -> (List, (Tree, Tree))
getTwo (Cons left (Cons right xs)) = (xs, (left, right))
getTwo Nil = print @String "Error: Empty stack on right subtree" ; (Nil, (Error, Error))
getTwo (Cons left Nil) = print @String "Error: Empty stack on left subtree" ; (Nil, (Error, left))

-- Streams

type Stream = &{
    NodeC: ?Int ; Stream
  , LeafC: Stream
  , EndOfStreamC: Wait
  }

-- Reading trees from channels

recTree : List -> Stream -> Tree
recTree xs (NodeC c) =
  let (xs, p) = getTwo xs in
  let (left, right) = p in
  let (root, c) = receive c in
  recTree (Cons (Node root left right) xs) c
recTree xs (LeafC c) =
  recTree (Cons Leaf xs) c
recTree xs (EndOfStreamC c) =
  wait c ; getFromSingleton xs

receiveTree : Stream -> Tree
receiveTree = recTree Nil

-- Go!

main : Tree
main =
  newHcServer @(Stream ; Wait) ("127.0.0.1", "8081") |>
  receiveTree
  -- let t = receiveTree r in repeat @() 10000 (\_:() -> ()) ; t
