mathServer : &{Opposite: ?Int;!Int;Skip, Plus: ?Int;?Int;!Int;Skip} -> Skip
mathServer c =
  match c with {
    Opposite c1 ->
      let n, c2 = receive c1 in
      send c2 (-n);
    Plus c1 ->
      let n1, c2 = receive c1 in
      let n2, c3 = receive c2 in
      send c3 (n1+n2)
  }
          
main : Int
main =
  let w,r = new +{Opposite: !Int;?Int;Skip, Plus: !Int;!Int;?Int;Skip} in
  let x = fork (mathServer r) in
  let w1 = select Plus w in
  let w2 = send w1 5 in
  let r1 = send w2 18 in
  let x, w1 = receive r1 in
  x
