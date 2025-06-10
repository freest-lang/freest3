type R = ?p(); Wait r
type S = ?q(); Wait s


f : R ->[top,bot] dualof S 1->[p,s] ()
f x y =
  let (n,x) = receive x in -- priority: p
  let y = send () y in     -- priority: q
  wait x;                  -- priority: p+1
  close y;                 -- priority: q+1
  ()

g : S ->[top,bot] dualof R 1->[q,r] ()
g y x =
  let (n,y) = receive y in -- priority: q
  let x = send () x in     -- priority: p
  wait y;                  -- priority: q+1
  close x;                 -- priority: p+1
  ()

main : ()
main =
  let (w1, r1)  = new @(dualof R) () in
  let (w2, r2)  = new @(dualof S) () in
  fork (\_:() 1-> f r1 w2);
  g r2 w1 