main : ()
main =
  let w, r = new !Int;?Bool;Skip in
  let x = fork (client w) in
  let n, r1 = receive r in
  let r2 = send r1 (n >= 0) in
  ()
  


client : !Int;?Bool;Skip -> ()
client c =
  let c1 = send c 5 in
  let b, c2 = receive c1
  in ()

