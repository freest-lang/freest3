-- VII exercise 2f

data Nat = Zero | Succ Nat

fact : Nat -> Nat
fact n = 
    case n of {
        Zero -> Succ Zero,
        Succ x -> mult n (fact x)
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

number3 : Nat
number3 = Succ (Succ (Succ Zero))

main : Nat
main = fact number3
--result = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))