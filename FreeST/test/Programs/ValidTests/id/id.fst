id' : forall a : TU . a -> a
id' x = x

main : Int
main = id'[Int] 5


