-- VIII exercise 5

numbers : [Int]
numbers = [1,2,3,4,5]

compare' : Int -> Int -> Int
compare' x y =
    if x < y 
        then -1
    else if x > y
        then 1
    else 0

guess : Int -> Int
guess x = guess' x 0

guess' : Int -> Int -> Int
guess' x y =
    let z = y + compare' x y in
        if x == z
            then z
            else guess' x z 
    
main : Int
main = guess 300
--result = 300