-- Streams

type Stream = +{
    NodeC: !Int ; Stream
  , LeafC: Stream
  , EndOfStreamC: Close
  }

-- Babdly behaving writer

writeRootTreeOnly : Stream -> ()

writeRootTreeOnly c =
  c |> select NodeC |> send 5 |> select EndOfStreamC |> close

-- Go!

main : ()
main =
  newHcClient @(Stream ; Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  writeRootTreeOnly -- "Error: Empty stack on right subtree"
