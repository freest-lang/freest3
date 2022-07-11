-- Type variables should not be shadowed by program variables.

-- implicit type abstraction
f : ∀ c . c -> Int
f c = f @c c

-- explicit type abstraction
trueC : ∀ a . a -> a -> a
trueC = \\a => \a:a -> \b:a -> a

main : Int 
main = trueC @Int 0 1