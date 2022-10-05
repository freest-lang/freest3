main : ()
main =
  let (w, r)  = new !Int;?Bool;End in
  fork @() (\_:()1-> client w); 
  let (n, r1) = receive r in
  let r2      = send (n >= 0) r1 in
  close r2


client : !Int;?Bool;End -> ()
client c =
  let c1      = send 5 c in
  let (b, c2) = receive c1 in
  close c2 
