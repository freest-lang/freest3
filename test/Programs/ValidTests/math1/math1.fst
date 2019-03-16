
mathServer : &{Opp: ?Int;!Int;Skip, Plus: ?Int;?Int;!Int;Skip} -> ()
mathServer c =
  match c with
    Opp c1 ->
      let n, c2 = receive c1 in
      let x = send (-n) c2 in
      ();

    Plus c1 ->
      let n1, c2 = receive c1 in
      let n2, c3 = receive c2 in
      let x = send (n1+n2) c3 in
      ()
        
          
main : Int
main =
  let w,r = new +{Opp: !Int;?Int;Skip, Plus: !Int;!Int;?Int;Skip} in
  let x = fork (mathServer r) in
  let w1 = select Plus w in
  let w2 = send 5 w1 in
  let r1 = send 18 w2 in
  let x, w1 = receive r1 in
  x
