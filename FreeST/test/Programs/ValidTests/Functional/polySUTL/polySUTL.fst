id' : forall a : TL . a -> a
id' x = x

f : Int -o Int
f x = 2 * x

main : Int
main =
  (id' @(Int -o Int) f) 5
