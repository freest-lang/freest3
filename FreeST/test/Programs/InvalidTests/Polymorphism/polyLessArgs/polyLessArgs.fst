fst' : forall a b . (a, b) -> a
fst' x =
  let (x1, x2) = x in
  x1

main : Int
main = fst' @Int (2, True)

