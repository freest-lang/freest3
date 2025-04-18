main : ()
main =
  newHcServer @(?() ; Close) ("127.0.0.1", "8081") |>
  receiveAndClose @()
