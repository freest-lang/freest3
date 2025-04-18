type MathClient = +{Negate: !Int;?Int, Add: !Int;!Int;?Int} ; Close

main : Int
main =
  newHcClient @MathClient (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  select Negate |> send 5 |> receiveAndClose @Int
