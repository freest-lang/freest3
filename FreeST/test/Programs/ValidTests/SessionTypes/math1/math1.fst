mathServer : &{Opposite: ?Int;!Int, Plus: ?Int;?Int;!Int} -> Skip
mathServer c =
  match c with {
    Opposite c1 ->
      let (n, c) = receive c1 in
      send (-n) c,
    Plus c1 ->
      let (n1, c2) = receive c1 in
      let (n2, c3) = receive c2 in
      send (n1 + n2) c3
  }

main : Int
main =
  let (w,r) = new +{Opposite: !Int;?Int, Plus: !Int;!Int;?Int} in
  let x = fork @Skip $ mathServer r in
  let w = select Plus w in
  let w = send 5 w in
  let w = send 18 w in
  let (x, _) = receive w in
  x
