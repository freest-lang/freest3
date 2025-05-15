-- Streams

type Stream = +{
    NodeC: !Int ; Stream
  , LeafC: Stream
  , EndOfStreamC: Close
  }

-- Babdly behaving writer

writeTooMuch : Stream -> ()

writeTooMuch c =
 c |>  select LeafC |> select LeafC |> select EndOfStreamC |> close

-- Go!

main : ()
main =
  newHcClient @(Stream ; Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  writeTooMuch      -- Error: Extraneous elements in the stream after reading a full tree
