f : forall a: 1S . a -> (Int, a)
f x = (7, x)

main : Int
main =
  let (s, r) = new !Char in
  let _ = fork @(Int, ?Char) (f  @(?Char) r) in
  let _ = send s 'a' in
  5
