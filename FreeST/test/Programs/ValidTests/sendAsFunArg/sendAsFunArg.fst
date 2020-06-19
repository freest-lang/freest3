type T = !Int

main : Int
main = 
  let (w, r) = new T in
  let _ = fork (f (send w)) in
  let (v, _) = receive r in
  v

f : (Int -o Skip) -> ()
f  g = 
  let _ = g 5 in ()
