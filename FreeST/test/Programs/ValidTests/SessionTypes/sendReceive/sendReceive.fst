main : ()
main =
  let (w, r)  = new !Int;?Bool;Skip in
  let x       = fork @() \_:() 1-> client w in
  let (n, r1) = receive r in
  let r2      = send (n >= 0) r1 in
  ()


client : !Int;?Bool;Skip -> ()
client c =
  let c1      = send 5 c in
  let (b, c2) = receive c1
  in ()
