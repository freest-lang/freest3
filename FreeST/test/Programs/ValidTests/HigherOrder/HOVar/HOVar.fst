f : ∀a . a -> (a, a)
f x =
  let (r, w) = new ?a;End in
  fork (send x w & close);
  let (y, r) = receive r in
  close r; 
  (y, y)

main : (Bool, Bool)
main =
  f @Bool True
