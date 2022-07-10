{- |
Module      : SystemFNats
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020

Church Encoding _ Natural Numbers
-}

type Nat = ∀ a . (a -> a) -> a -> a

zero, zero', one, two, three, four : Nat

zero = Λ a => λs:(a -> a) -> λz:a -> z

-- Abbreviated version
zero' s z = z

one s z = s z

two s z = s (s z)

three s z = s (s (s z))

four = succ' three

succ', square : Nat -> Nat
succ' n = Λ a => λ s:(a->a) z:a -> s (n  @a s z)

square n = Λ a => λ s:(a -> a) z:a -> n  @a (n  @a s) z

plus, plus', times, exp : Nat -> Nat -> Nat
plus m n = m  @Nat succ' n

plus' m n = Λ a => λ s:(a->a) z:a -> m  @a s (n  @a s z)

times m n = Λ a => λs:(a->a) -> n  @a (m  @a s)

exp m n = Λ a => λ f:(a -> a) -> n  @(a -> a) (m  @a) f

isZero : Nat -> Bool
isZero n = n  @Bool (λ_:Bool -> False) True

{-
Note: One cannot apply eta-reduction in the code above to get rid
of f and obtain:

  exp m n = Λ a => n [a->a] (m [a])

for the body of the type application (that is, n [a->a] (m [a]))
must be a value. Once we apply eta-expansion we obtain Λ a => λ f...
which is OK for λ f... is a value.
-}

-- Pairs of natural numbers for the predecessor

type Pair = (Nat -> Nat -> Nat) -> Nat

fst', snd' : Pair -> Nat
fst' p = p (λ m:Nat _:Nat -> m)

snd' p = p (λ _:Nat n:Nat -> n)

pair : Nat -> Nat -> Pair
pair m n = λz:(Nat -> Nat -> Nat) -> z m n

shift : Pair -> Pair
shift p = pair (succ' (fst' p)) (fst' p)

pred' : Nat -> Nat
pred' n = snd' (n  @Pair shift (pair zero zero))

-- Testing

toInt : Nat -> Int
toInt n = n  @Int (λx:Int -> x + 1) 0

main : Int
main = toInt $ pred' $ plus one three
-- main = toInt $ exp two $ times four four

-- main : Bool
-- main = isZero $ plus three four
