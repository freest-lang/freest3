
-- TODO: recursiveVariation
-- type DD = dualof (dualof !Int;DD)
type DD : 1S = dualof (dualof !Int)

sendInt : ∀ a:1S . DD;a -> a
sendInt c = send 5 c


rcvInt : ∀ a:1S . (dualof DD);a -> (Int, a)
rcvInt c = receive c


main : Int
main =
  let (w,r) = new @(DD;EndC) () in
  fork @() (\_:()1-> sendInt @EndC w |> close);
  let (i, r) = rcvInt @EndW r in
  wait r;
  i
