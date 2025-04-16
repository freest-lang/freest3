type T = &{More: ?Int;T, Stop: Wait}

g : T -> Int
g r =
  match r with {
   	More r ->
      let (v, r) = receive r in
      v + g r,
    Stop r -> wait r; 0
  }


main : Int
main =
  newHcServer @T ("127.0.0.1", "8081") |>
  g