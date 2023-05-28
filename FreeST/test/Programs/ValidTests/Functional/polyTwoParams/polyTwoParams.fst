mkPair : forall a : 1T . forall b : 1T . a -> b 1-> (a, b)
mkPair x y = (x, y)

main : (Int, Bool)
main =
  let (r, w) = new @(Skip;EndW) () in
  let (i, s) = mkPair @Int @(Skip;EndW) 4 r in
  fork (\_:() 1-> close w);
  fork (\_:() 1-> wait s);
  mkPair @Int @Bool i True
