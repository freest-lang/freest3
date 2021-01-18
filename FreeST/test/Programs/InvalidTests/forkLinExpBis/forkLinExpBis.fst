f : forall a:SL . a -> (Int, a)
f x = (7, x)

main : Int
main =
  let (s, r) = new !Char in
  let _ = fork (f [?Char] r) in
  let _ = send s 'a' in
  5
