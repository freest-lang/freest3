-- VII exercise 3

data Integer = I Int

add : Integer -> Integer -> Integer
add x y = case x of { I a -> case y of { I b -> I (a+b) }}

monus : Integer -> Integer -> Integer
monus x y = 
    case x of { 
        I a -> 
            case y of { 
                I b -> if b > a then I 0 else I (a-b) 
            }
    }

pred' : Integer -> Integer
pred' x = case x of { I a -> I (a-1) }

sub : Integer -> Integer -> Integer
sub x y = 
    case y of {
        I a -> 
            if a == 0
                then x
                else sub (pred' x) (pred' y)
    }

mult : Integer -> Integer -> Integer
mult x y =
    case y of {
        I b -> 
            if b == 0
                then I 0
                else add x (mult x (pred' y))
    }

fact : Integer -> Integer
fact x =
    case x of {
        I n -> 
            if n == 0 
                then (I 1) 
                else mult x (fact (pred' x))
    }

remnat : Integer -> Integer -> Integer
remnat x y = case x of { I a -> case y of { I b -> I (mod a b) }}

quotnat : Integer -> Integer -> Integer
quotnat x y = case x of { I a -> case y of { I b -> I (div a b) }}

lessThan : Integer -> Integer -> Bool
lessThan x y = case x of { I a -> case y of { I b -> a < b }}

main : Bool
main = 
    let zero = I 0 in       let one = I 1   in
    let two = add one one                   in
    let one1 = pred' two                    in
    let one2 = sub two one                  in
    let three = add two one                 in
    let four = mult two two                 in 
    let fourFact = fact four                in
    let t = add fourFact (add four (add three (add one2 (add one1 (add two (add one zero)))))) in
    -- t = I 36
    let r1 = remnat four three              in -- I 1
    let r2 = remnat t three                 in -- I 0
    let q = quotnat t three                 in -- I 12
    let s = add r1 (add r2 q)               in -- I 13
    lessThan s t
--result = True
