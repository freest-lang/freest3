mathServer : &{Opposite: ?Int;!Int, Plus: ?Int;?Int;!Int} -> Skip
mathServer c =
  match c with {
    Opposite c ->
      let (n, c) = receive c in
      send (-n) c,
    Plus c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      send (n1 + n2) c
  }

main : Int
main =
  let (r,w) = new &{Opposite: ?Int;!Int, Plus: ?Int;?Int;!Int} in
  let _ = fork[Skip] (mathServer r) in
  let (x, _) = receive (send 5 (select Opposite w)) in
  x
