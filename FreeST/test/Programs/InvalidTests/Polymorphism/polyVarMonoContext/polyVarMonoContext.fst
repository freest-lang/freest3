f : forall a : *T . a -> a
f x = x

main : Int
main =  f (f 5)
