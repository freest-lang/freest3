main : Bool
main =
  let (w, r) = new ?Int;!Bool;Skip in
  let x = fork (client w) in
  let c1 = send r (-5) in
  let (b, c2) = receive c1
  in b


client : ?Int;!Bool;Skip -> ()
client c =
  let (n, r1) = receive c in
  let r2 = send r1 (n >= 0) in
  ()  
