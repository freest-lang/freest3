-- V exercise 14

minus : Int -> Int -> Int
minus x y = x - y

orderInverter : (Int -> Int -> Int) -> Int -> Int -> Int
orderInverter f x y = f y x

main : Int
main = orderInverter minus 10 20
--result = 10