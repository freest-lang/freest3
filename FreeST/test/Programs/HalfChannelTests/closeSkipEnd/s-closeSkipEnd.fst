main : ()
main =
  newHcServer @(Skip;Wait) ("127.0.0.1", "8081") |>
  wait
