f : ∀a . a -> (a, a)
f x =
  let (r, w) = new ?a in
  send x w;
  let (y, _) = receive r in
  (y, y)

main : (Bool, Bool)
main =
  f [Bool] True
