main : ()
main =
  newHcClient1 @(!String ; Close) ("127.0.0.1", "8081") |>
  sendAndClose @String "Hello"
