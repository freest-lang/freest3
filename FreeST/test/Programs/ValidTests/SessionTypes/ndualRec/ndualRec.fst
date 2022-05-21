type Choice : 1S = +{More: !Int;DD, Enough: Skip}
type DD : 1S = dualof (dualof Choice)

sendInt : Int -> DD -> Skip
sendInt i c =
    let c = select More c in
    let c = send i c in
    let c = select More c in
    let c = send (i + 1) c in
    let c = select More c in
    let c = send (i + 2) c in
    select Enough c

rcvInt : Int -> dualof DD -> (Int, Skip)
rcvInt acc c =
  match c with {
    Enough c -> (acc,c),
    More c ->
      let (i, c) = receive c in
      let (iii, c) = rcvInt (acc+i) c in
      (iii, c)
  }

main : Int
main =
  let (w,r) = new DD in
  let _ = fork @Skip \_:() 1-> sendInt 0 w in
  let (i, _) = rcvInt 0 r in
  i
