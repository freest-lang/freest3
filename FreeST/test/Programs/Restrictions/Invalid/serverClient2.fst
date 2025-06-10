type R = ?p(); Wait p+2
type S = ?q(); Wait q+2


f : R ->[top,bot] dualof S 1->[p,q+2] ()
f x y =
  let (n,x) = receive x in -- priority: p
  let y = send () y in     -- priority: q
  wait x;                  -- priority: p+2
  close y;                 -- priority: q+2
  ()

g : S ->[top,bot] dualof R 1->[q,p+2] ()
g y x =
  let (n,y) = receive y in -- priority: q
  let x = send () x in     -- priority: p
  wait y;                  -- priority: q+2
  close x;                 -- priority: p+2
  ()

main : ()
main =
  let (w1, r1)  = new @(dualof R) () in
  let (w2, r2)  = new @(dualof S) () in
  fork (\_:() 1-> f r1 w2);
  g r2 w1 