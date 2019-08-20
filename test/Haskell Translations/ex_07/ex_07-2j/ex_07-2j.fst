-- VII exercise 2j

data Nat = Zero | Succ Nat

lessThan : Nat -> Nat -> Bool
lessThan x y =
    case y of {
        Zero -> False,
        Succ b ->
            case x of {
                Zero -> True,
                Succ a -> lessThan a b
            }
    }

number2 : Nat
number2 = Succ (Succ Zero)

main : Bool
main = lessThan number2 (Succ (Succ (Succ number2)))
--result = True