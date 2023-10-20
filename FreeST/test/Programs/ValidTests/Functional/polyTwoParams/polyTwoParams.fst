mkPair : forall a . forall b . a -> b 1-> (a, b)
mkPair x y = (x, y)

main : (Int, Bool)
main =
  let (r, w) = new @(Skip;End) () in
  let (i, s) = mkPair @Int @(Skip;End) 4 r in
  fork (\_:() 1-> close w);
  fork (\_:() 1-> close s);
  mkPair @Int @Bool i True

