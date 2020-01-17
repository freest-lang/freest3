-- VII exercise 2d

data Nat = Zero | Succ Nat

sub : Nat -> Nat -> Nat
sub x y =
    case x of {
        Zero -> Zero,
        Succ a -> 
            case y of {
                Zero -> x,
                Succ b -> sub (pred' x) b
            }
    }

pred' : Nat -> Nat
pred' n =
    case n of {
        Zero -> Zero,
        Succ a -> a
    }
    

number5 : Nat
number5 = Succ (Succ (Succ (Succ (Succ Zero))))

number2 : Nat
number2 = Succ (Succ Zero)

main : Nat
main = sub number5 number2
--result = Succ (Succ (Succ Zero))