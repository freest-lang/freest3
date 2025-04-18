main : ()
main =
  newHcClient1 @(!() ; Wait) ("127.0.0.1", "8081") |>
  sendAndWait @() ()
