f : ?Int -o (Int, Skip)
f = receive

main : (Int, Skip)
main =
  let (r, w) = new !Int in
  let _ = fork[Skip] $ send 5 r in
  f w
