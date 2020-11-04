-- type Choice : SL = +{More: !Int;DD, Enough: ?Int} --TODO: TEST OTHER VERSION

--type Choice : SL = +{More: !Int;Choice, Enough: Skip}
type Choice : SL = +{More: !Int;DD, Enough: Skip}
type DD : SL = dualof (dualof Choice)

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
  let _ = fork (sink (sendInt 0 w)) in
  let (i, _) = rcvInt 0 r in
  i

sink : Skip -> ()
sink _ = ()


-- tmp/ndualRec.fst:29:29: error:
-- 	 Couldn't match expected type ((dualof DD);Skip)
-- 	             with actual type (dualof DD)
-- 	               for expression r
