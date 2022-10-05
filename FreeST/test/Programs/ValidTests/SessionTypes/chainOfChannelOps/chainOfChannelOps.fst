type T : 1S = +{More: !Int;T, Stop: End}

main : Int
main =
  let (w, r) = new T in
  fork @() (\_:()1-> select More w |> send 5 |> select More |> send 2 |> select Stop |> close);
  g r


g : dualof T -> Int
g r =
  match r with {
   	More r ->
      let (v, r) = receive r in
      v + g r,
    Stop r -> close r; 0
  }
