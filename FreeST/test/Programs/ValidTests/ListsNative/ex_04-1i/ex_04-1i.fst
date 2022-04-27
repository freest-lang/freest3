-- IV exercise 1i

potencias : Int -> [Int] -> [Int]
potencias b exp =
    case exp of {
        [] -> [],
        e :: rest -> (pow b e) :: (potencias b rest)
    }

pow : Int -> Int -> Int
pow b e = if e == 0 then 1 else b * (pow b (e-1))

main : [Int]
main = potencias 3 ([1,2,3,4,5,6,7,8,9,10])
-- result = [3,9,27,81,243,729,2187,6561,19683,59049]