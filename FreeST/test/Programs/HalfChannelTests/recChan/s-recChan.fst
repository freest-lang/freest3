type Chan = &{Done: Wait, More: ?Float ; Chan}

sumFives : Chan -> Float
sumFives c =
  match c with {
    Done c -> wait c; 0.0,
    More c ->
     let (n, c) = receive c in
     n +. sumFives c
  }

main : Float
main =
  newHcServer @Chan ("127.0.0.1", "8081") |>
  sumFives
