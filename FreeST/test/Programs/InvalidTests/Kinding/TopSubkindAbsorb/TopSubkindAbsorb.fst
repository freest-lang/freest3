f : forall a:1A . a -> ()
f x = f @a x

g : (Int 1-> Bool) -> ()
g = f @ (Int 1-> Bool)
