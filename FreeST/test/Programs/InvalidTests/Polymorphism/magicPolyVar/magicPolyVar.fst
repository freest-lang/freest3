{- |
Module      : SystemFNats
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020

Church Encoding _ Natural Numbers
-}

type Nat = ∀ a . (a -> a) -> a -> a

-- Wrong: type variable a is free in the body of the function
succ' : Nat -> Nat
succ' n s z = s (n  @a s z)

-- Right
succ'' : Nat -> Nat
succ'' n = Λ a => λ s:(a->a) z:a -> s (n  @a s z)

-- Right
succ''' : Nat -> Nat
succ''' n = Λ b => λ s:(b->b) z:b -> s (n  @b s z)

main : Int
main = 5
