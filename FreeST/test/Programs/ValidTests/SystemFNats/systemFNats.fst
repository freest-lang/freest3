{- |
Module      : SystemFNats
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020

Church Encoding _ Natural Numbers
-}

type Nat = ∀ a . (a -> a) -> a -> a

zero : Nat
zero = Λ a => λs:(a -> a) -> λz:a -> z

zero' : Nat -- Abbreviated version
zero' s z = z

one : Nat
one s z = s z

two : Nat
two s z = s (s z)

three : Nat
three s z = s (s (s z))

succ : Nat -> Nat
succ n = Λ a => λ s:(a->a) z:a -> s (n [a] s z)

four : Nat
four = succ three

plus : Nat -> Nat -> Nat
plus m n = Λ a => λ s:(a->a) z:a -> m [a] s (n [a] s z)

isZero : Nat -> Bool
isZero n = n [Bool] (λ_:Bool -> False) True

times : Nat -> Nat -> Nat
times m n = Λ a => λs:(a->a) -> n [a] (m [a] s)

exp : Nat -> Nat -> Nat
exp m n = Λ a => n [a->a] (m [a])

square : Nat -> Nat
square n = Λ a => λ s:(a -> a) z:a -> n [a] (n [a] s) z

-- Pairs of natural numbers for the predecessor

type Pair = (Nat -> Nat -> Nat) -> Nat

fst : Pair -> Nat
fst p = p (λ m:Nat _:Nat -> m)

snd : Pair -> Nat
snd p = p (λ _:Nat n:Nat -> n)

pair : Nat -> Nat -> Pair
pair m n = λz:(Nat -> Nat -> Nat) -> z m n

shift : Pair -> Pair
shift p = pair (succ (fst p)) (fst p)

pred : Nat -> Nat
pred n = snd (n [Pair] shift (pair zero zero))

-- Testing

toInt : Nat -> Int
toInt n = n [Int] (λx:Int -> x + 1) 0

main : Int
main = toInt $ pred four
-- main = toInt $ exp two $ times four four

-- main : Bool
-- main = isZero $ plus three four
