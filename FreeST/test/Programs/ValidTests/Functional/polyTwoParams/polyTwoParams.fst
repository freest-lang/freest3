mkPair : forall a : 1T . forall b : 1T . a -> b 1-> (a, b)
mkPair x y = (x, y)

main : (Int, Bool)
main =
  let (r, w) = new Skip in
  let (i, s) = mkPair[Int][Skip] 4 r in
  mkPair[Int][Bool] i True

