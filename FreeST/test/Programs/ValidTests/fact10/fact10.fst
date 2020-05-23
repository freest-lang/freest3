type Choice : SL = +{More: !Int;Choice, Enough: Skip}

sendInt : Int -> Choice -> Skip
sendInt i c =
  if i == 0 then
    select c Enough
  else
    let c = select c More in
    let c = send c i in -- change i to another value
    sendInt (i-1) c
  
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
  let (w,r) = new Choice in
  let _ = fork (sendInt 10 w) in
  let (i, _) = rcvInt 1 r in
  i
