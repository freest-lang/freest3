-- VII exercise 2c

data Nat = Zero | Succ Nat

pred' : Nat -> Nat
pred' n =
    case n of {
        Zero -> Zero,
        Succ a -> a
    }

number5 : Nat
number5 = Succ (Succ (Succ (Succ (Succ Zero))))

main : Nat
main = pred' number5
--result = Succ (Succ (Succ (Succ Zero)))
