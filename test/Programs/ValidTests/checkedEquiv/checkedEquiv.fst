f : Skip ; (Skip; !Int) -> Int -o Skip
f c = send c

main : (Int, Skip)
main =
  let (r, w) = new !Int in
  let _ = fork (f r 5) in
  receive w
