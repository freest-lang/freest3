-- Creates an unbounded number of channels; diverges
write : !Int -o Int -> Char
write c n =
  let _ = send n c in
  printIntLn n ;
  let (r, w) = new !Int in
  let _ = fork[(Int, Skip)] $ receive w in
  write r (n + 1)

main : Char
main =
  let (r, w) = new !Int in
  let _ = fork[(Int, Skip)] $ receive w in
  write r 0
