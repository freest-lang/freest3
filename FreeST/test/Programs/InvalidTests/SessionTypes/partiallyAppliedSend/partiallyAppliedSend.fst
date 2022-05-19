f : Int -> !Int 1-> Skip
f = send

main : (Int, Skip)
main =
  let (r, w) = new !Int in
  let _ = fork[Skip] $ f 5 r in
  receive w
