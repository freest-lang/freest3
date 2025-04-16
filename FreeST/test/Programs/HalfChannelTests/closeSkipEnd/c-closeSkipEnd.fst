main : ()
main =
  newHcClient1 @(Skip;Close) ("127.0.0.1", "8081") |>
  close
