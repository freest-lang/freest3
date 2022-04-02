mkPair : forall a : TL . forall b : TL . a -> b -o (a, b)
mkPair x y = (x, y)

main : (Int, Bool)
main =
  let (r, w) = new Skip in
  let (i, s) = mkPair @Int @Skip 4 r in
  mkPair @Int @Bool i True

