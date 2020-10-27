-- type Choice : SL = +{More: !Int;DD, Enough: ?Int} --TODO: TEST OTHER VERSION

--type Choice : SL = +{More: !Int;Choice, Enough: Skip}
type Choice : SL = +{More: !Int;DD, Enough: Skip}
type DD : SL = dualof (dualof (dualof (dualof Choice)))

sendInt : Int -> DD -> Skip
sendInt i c =
    let c = select c More in
    let c = send c i in
    let c = select c More in
    let c = send c (i+1) in
    let c = select c More in
    let c = send c (i+2) in
    select c Enough

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
  let _ = fork (sink (sendInt 0 w)) in
  let (i, _) = rcvInt 0 r in
  i

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
