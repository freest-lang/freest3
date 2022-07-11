-- VII exercise 2i

data Nat = Zero | Succ Nat

quotnat : Nat -> Nat -> Nat
quotnat n d = 
    if isZero (monus n d)
        then (if isZero (monus d n)
                then Succ Zero
                else Zero)
        else Succ (quotnat (monus n d) d)

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

isZero : Nat -> Bool
isZero n = 
    case n of {
        Zero -> True,
        Succ _ -> False
    }

number5 : Nat
number5 = Succ (Succ (Succ (Succ (Succ Zero))))

number2 : Nat
number2 = Succ (Succ Zero)

main : Nat
main = quotnat number5 number2
--result = Succ (Succ Zero)
