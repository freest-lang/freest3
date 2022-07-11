-- VII exercise 2a

data Nat = Zero | Succ Nat

add : Nat -> Nat -> Nat
add x y =
    case x of {
        Zero -> y,
        Succ z -> Succ (add z y)
    }

number2 : Nat
number2 = Succ (Succ Zero)

main : Nat
main = add number2 number2
--result = Succ (Succ (Succ (Succ Zero)))
