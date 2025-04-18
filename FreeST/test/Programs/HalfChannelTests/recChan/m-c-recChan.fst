type Chan = +{Done: Close, More: !Float;Chan}

fives : Int -> Chan -> ()
fives n c =
  if n == 0
  then select Done c |> close
  else fives (n-1) (select More c |> send (-5.0))

main : ()
main =
  newHcClient @Chan (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  fives 32 
