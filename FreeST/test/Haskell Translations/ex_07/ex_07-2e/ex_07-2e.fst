-- VII exercise 2e

data Nat = Zero | Succ Nat

mult : Nat -> Nat -> Nat
mult x y =
    case y of {
        Zero -> Zero,
        Succ b -> add x (mult x b)
    }

add : Nat -> Nat -> Nat
add x y =
    case x of {
        Zero -> y,
        Succ z -> Succ (add z y)
    }

number3 : Nat
number3 = Succ (Succ (Succ Zero))

number2 : Nat
number2 = Succ (Succ Zero)

main : Nat
main = mult number3 number2
--result = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))