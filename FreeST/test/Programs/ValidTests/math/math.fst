mathServer : &{Opposite: ?Int;!Int, Plus: ?Int;?Int;!Int} -> Skip
mathServer c =
  match c with {
    Opposite c ->
      let (n, c) = receive c in
      send c (-n),
    Plus c ->
      let (n1, c) = receive c in
      let (n2, c) = receive c in
      send c (n1 + n2)
  }

main : Int
main =
  let (r,w) = new &{Opposite: ?Int;!Int, Plus: ?Int;?Int;!Int} in
  let _ = fork (sink (mathServer r)) in
  let (x, _) = receive (send (select w Opposite) 5) in
  x

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
