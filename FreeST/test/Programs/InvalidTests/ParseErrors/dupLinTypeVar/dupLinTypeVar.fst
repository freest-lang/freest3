f : forall a : 1T . a -> (a, a)
f x = (x, x)

main : (Int, Int)
main = f @Int 5
