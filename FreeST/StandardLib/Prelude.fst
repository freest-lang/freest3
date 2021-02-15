

-- | Prelude

id : ∀ a . a → a
id = Λ a => λ x:a -> x

flip : ∀ a b c . (a → b → c) → b → a → c
flip f x y = f y x

until : ∀ a . (a → Bool) → (a → a) → a → a
until p f x = if p x then x else until[a] p f (f x)

-- | 'curry' converts an uncurried function to a curried function.
-- curry fst 1 2
-- 1

curry : ∀ a b c . ((a, b) → c) → a → b → c
curry f x y =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
-- uncurry (+) (1,2)
-- 3

uncurry  : ∀ a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst[a,b] p) (snd[a,b] p)

-- | Swap the components of a pair.
swap : ∀ a b . (a,b) → (b,a)
swap x = let (a,b) = x in (b,a)
