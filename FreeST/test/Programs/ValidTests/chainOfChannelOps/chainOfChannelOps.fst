type T : SL = +{More: !Int;T, End: Skip}

main : Int
main =
  let (w, r) = new T in
  let _ = fork[Skip] $ select End $ send 2 $ select More $ send 5 $ select More w in
  g r


g : dualof T -> Int
g r =
  match r with {
   	More r ->
      let (v, r) = receive r in
      v + g r,
    End r -> 0
  }
