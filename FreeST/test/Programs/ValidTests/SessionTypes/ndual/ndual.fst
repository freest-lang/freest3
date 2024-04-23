
-- TODO: recursiveVariation
-- type DD = dualof (dualof !Int;DD)
type DD = dualof (dualof !Int)

sendInt : DD;a -> a
sendInt c = send 5 c


rcvInt : (dualof DD);a -> (Int, a)
rcvInt c = receive c


main : Int
main =
  let (w,r) = new @(DD;Close) () in
  fork @() (\_:()1-> sendInt @Close w |> close);
  let (i, r) = rcvInt @Wait r in
  wait r;
  i
