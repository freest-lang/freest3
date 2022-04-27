-- V exercise 20

indexOf : Int -> [Int] -> Int 
indexOf e list = 
    case list of {
        [] -> -1,
        x :: rest -> 
            if e == x 
                then 0
                else 1 + (indexOf e rest)
    }

list : [Int]
list = [1,2,3,4,5]

main : Int
main = indexOf 4 list
--result = 3