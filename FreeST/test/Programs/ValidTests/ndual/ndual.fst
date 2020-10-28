
-- TODO: recursiveVariation
-- type DD = dualof (dualof !Int;DD)
type DD = dualof (dualof !Int)

sendInt : DD -> Skip
sendInt c = send 5 c

rcvInt : dualof DD -> Int
rcvInt c =
  let (i, _) = receive c in
  i


main : Int
main =
  let (w,r) = new DD in
  let _ = fork (sink (sendInt w)) in
  rcvInt r

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
