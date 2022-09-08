main : Bool
main =
  let (w, r) = new ?Int;!Bool;End in
  let x = fork @() (client w) in
  let c1 = send (-5) r in
  let (b, c2) = receive c1 in 
  close c2;
  b


client : ?Int;!Bool;End -> ()
client c =
  let (n, r1) = receive c in
  let r2 = send (n >= 0) r1 in
  close r2
