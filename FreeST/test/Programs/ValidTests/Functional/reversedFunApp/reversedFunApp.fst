f : ∀a . ∀b . a -> (a -> b) -> b
f = (|>)

main : Int
main = f @Int @Int 5 (\x:Int -> x)

