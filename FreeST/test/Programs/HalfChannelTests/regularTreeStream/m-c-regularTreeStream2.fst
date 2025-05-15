-- Streams

type Stream = +{
    NodeC: !Int ; Stream
  , LeafC: Stream
  , EndOfStreamC: Close
  }

-- Babdly behaving writer

writeNothing : Stream -> ()
writeNothing c =
  c |> select EndOfStreamC |> close

-- Go!

main : ()
main =
  newHcClient @(Stream ; Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  writeNothing
