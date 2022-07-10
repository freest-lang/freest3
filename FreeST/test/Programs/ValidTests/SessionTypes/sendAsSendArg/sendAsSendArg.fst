f1 : !Int;!Int -> Skip
f1 c = send 5 (send 5 c)

f2 : ?Int;?Int -> Int
f2 c =
  let (x1, c) = receive c in
  let (x2, c) = receive c in
  x1 + x2

main : Int
main =
  let (c1, c2) = new !Int;!Int in
  let _ = fork @Skip (f1 c1) in
  f2 c2 -- in x
