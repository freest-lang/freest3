type RcvInt : 1S = ?Int
type Arrow = Int -> dualof RcvInt -> Int

sendInt : Arrow
sendInt i c = let _ = send i c in 0 -- zero just for test purposes

rcvInt : dualof Arrow
rcvInt i c =
  let (j, c) = receive c in
  j


main : Bool
main = False
