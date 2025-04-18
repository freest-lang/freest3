type MathServer = &{Negate: ?Int;!Int, Add: ?Int;?Int;!Int} ; Wait

mathServer : MathServer-> ()
mathServer c =
  match c with {
    Negate c ->
      let (n, c) = receive c in
      c |> send (-n) |> wait,
    Add c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      c |> send (n1 + n2) |> wait
  }

main : ()
main =
  newHcServer @MathServer ("127.0.0.1", "8081") |>
  mathServer 
