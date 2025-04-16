main : ()
main =
  newHcClient @(Skip;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  close
