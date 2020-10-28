type Choice : SL = +{More: !Int;Choice, Enough: Skip}

sendInt : Int -> Choice -> Skip
sendInt i c =
  if i == 0 then
    select Enough c
  else
    let c = select More c in
    let c = send c i in
    sendInt (i - 1) c

rcvInt : Int -> dualof Choice -> (Int, Skip)
rcvInt acc c =
  match c with {
    Enough c -> (acc,c),
    More c ->
      let (i, c) = receive c in
      let (iii, c) = rcvInt (acc*i) c in
      (iii, c)
  }

main : Int
main =
  let (w, r) = new Choice in
  let _ = fork (sink (sendInt 10 w)) in
  let (i, _) = rcvInt 1 r in
  i

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
