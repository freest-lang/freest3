-- I exercise 3

addDigit : Int -> Int -> Int
addDigit a b = a * (pow 10 (numberLength b)) + b

numberLength : Int -> Int
numberLength x = if ((div x 10) == 0) then 1 else 1 + numberLength (div x 10)

pow : Int -> Int -> Int
pow b e = if e <= 0 then 1 else b * (pow b (e-1))
-- e <= 0 for simplicity

main : Int
main = addDigit 12 3456789
-- should work for any positive integer
-- result = 123456789
