mkPair : forall a : TL, b : TL => a -> b -> (a, b)
mkPair x y = x

main : (Int, Bool)
main =
  let r, w = new Skip in
  let i, s = mkPair[Int, Skip] 4 r in
  mkPair[Int, Bool] i True

