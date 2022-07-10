-- VII exercise 2g

data Nat = Zero | Succ Nat

pot : Nat -> Nat -> Nat
pot b e =
    case e of {
        Zero -> Succ Zero,
        Succ n -> mult b (pot b n)
    }

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

number2 : Nat
number2 = Succ (Succ Zero)

main : Nat
main = pot number2 number2
--result = Succ (Succ (Succ (Succ Zero)))
