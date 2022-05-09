f : forall a : TU . a -> a
f x = x

main : Int
main =  f (f 5)
