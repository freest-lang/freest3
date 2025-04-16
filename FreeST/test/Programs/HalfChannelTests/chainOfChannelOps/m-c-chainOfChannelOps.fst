type T = +{More: !Int;T, Stop: Close}

main : ()
main =
  newHcClient @T (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  select More |> send 5 |> select More |> send 2 |> select Stop |> close;
  ()