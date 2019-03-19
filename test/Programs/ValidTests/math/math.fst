
mathServer : &{Opp: ?Int;!Int;Skip, Plus: ?Int;?Int;!Int;Skip} -> ()
mathServer c =
  match c with {
    Opp c1 ->
      let n, c2 = receive c1 in
      let x = send c2 (-n) in
      ();
      
    Plus c1 ->
      let n1, c2 = receive c1 in
      let n2, c3 = receive c2 in
      let x = send c3 (n1+n2) in
      ()  
  }
          
main : Int
main =
  let r,w = new &{Opp: ?Int;!Int;Skip, Plus: ?Int;?Int;!Int;Skip} in
  let x = fork (mathServer r) in
  let w1 = select Opp w in
  let r1 = send w1 5 in
  let x, w1 = receive r1 in
  x


  
 
        
