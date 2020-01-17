f : forall a : TL => a -> a
f x = (x, x)

main : (Int, Int)
main = f[Int] 5
