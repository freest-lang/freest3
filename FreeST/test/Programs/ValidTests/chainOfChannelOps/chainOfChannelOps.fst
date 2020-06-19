type T : SL = +{More: !Int;T, End: Skip}

main : Int
main = 
  let (w, r) = new T in
  let _ = fork (select (send (select (send (select w More) 5) More) 2) End) in
  g r


g : dualof T -> Int
g r =
  match r with {
   	More r -> 
      let (v, r) = receive r in
      v + g r,
    End r -> 0
  }		
