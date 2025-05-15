-- Integer trees

data Tree = Leaf | Node Int Tree Tree

-- Example:
--               1
--        2            6
--    8      3            7
--         5   4
aTree : Tree
aTree = Node 1 (Node 2 (Node 8 Leaf Leaf) (Node 3 (Node 5 Leaf Leaf) (Node 4 Leaf Leaf))) (Node 6 Leaf (Node 7 Leaf Leaf))

-- Streams

type Stream = +{
    NodeC: !Int ; Stream
  , LeafC: Stream
  , EndOfStreamC: Close
  }

-- Writing trees on channels

streamTree : Tree -> Stream -> Stream
streamTree Leaf c = select LeafC c
streamTree (Node x l r) c = send x $ select NodeC $ streamTree r $ streamTree l c
streamTree Error c = select LeafC c

sendTree : Tree -> Stream -> ()
sendTree t c = c |> streamTree t |> select EndOfStreamC |> close

main : ()
main =
  newHcClient @(Stream ; Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  sendTree aTree    -- No error
