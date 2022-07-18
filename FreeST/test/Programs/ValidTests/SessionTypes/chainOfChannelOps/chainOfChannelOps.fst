type T : 1S = +{More: !Int;T, Stop: Skip}

main : Int
main =
  let (w, r) = new T in
  let _ = fork @Skip $ select Stop $ send 2 $ select More $ send 5 $ select More w in
  g r


g : dualof T -> Int
g r =
  match r with {
   	More r ->
      let (v, r) = receive r in
      v + g r,
    Stop r -> 0
  }
