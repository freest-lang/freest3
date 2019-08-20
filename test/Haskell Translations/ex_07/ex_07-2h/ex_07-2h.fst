-- VII exercise 2h

data Nat = Zero | Succ Nat

remnat : Nat -> Nat -> Nat
remnat n d = 
    if isZero (monus n d)
        then (if isZero (monus d n)
                then Zero
                else n)
        else remnat (monus n d) d

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
main = remnat number5 number2
--result = Succ Zero