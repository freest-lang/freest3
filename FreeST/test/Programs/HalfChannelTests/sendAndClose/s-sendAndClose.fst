main : String
main =
  newHcServer @(?String ; Wait) ("127.0.0.1", "8081") |>
  receiveAndWait @String
