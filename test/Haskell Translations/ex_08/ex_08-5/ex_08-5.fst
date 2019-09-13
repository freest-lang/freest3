-- VIII exercise 5

data List = E | L Int List

numbers : List
numbers = L 1 (L 2 (L 3 (L 4 (L 5 E))))

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