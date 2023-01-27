myNew : forall a:1S . () -> (a, dualof a)
myNew _ = new @a ()

main : (Skip, Skip)
main = myNew @Skip ()

{-

-- __dollar : ∀ a b . (a -> b) -> a -> b
-- __dollar f x = f x

passDollar : (∀ a b . (a -> b) -> a -> b) -> Bool
passDollar dollar = dollar @Int @Bool (>0) 5

main' : Bool
main' = passDollar __dollar

--

-- __triangle : ∀ a b . a -> (a -> b) -> b
-- __triangle x f = f x

passTriangle : (∀ a b . a -> (a -> b) -> b) -> Bool
passTriangle triangle = triangle @Int @Bool 5 (<0)

main : Bool
main = passTriangle (|>)

--

-- __semicolon : ∀a:*T . ∀b:1T . a -> b -> b
-- __semicolon x y = x ; y

passSemi : (∀a:*T . ∀b:1T . a -> b -> b) -> Char
passSemi semi = semi @Int @Char 5 'a'

main'' : Char
main'' = passSemi __semicolon

-}
