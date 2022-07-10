-- VII exercise 2b

data Nat = Zero | Succ Nat

monus : Nat -> Nat -> Nat
monus x y = 
    case y of {
        Zero -> x,
        Succ b -> 
            case x of {
                Zero -> Zero,
                Succ a -> monus a b
            }
    }

number5 : Nat
number5 = Succ (Succ (Succ (Succ (Succ Zero))))
number2 : Nat
number2 = (Succ (Succ Zero))

main : Nat
main = monus number5 number2
--result = Succ (Succ (Succ Zero))
