main : ()
main =
  newHcClient @(!String ; Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  sendAndClose @String "Hello"
