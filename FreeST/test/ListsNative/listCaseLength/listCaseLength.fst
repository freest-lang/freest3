
l : [Int]
l = [1,2,3,4,5,6,7,8,9,10]

length : [Int] -> Int
length l =
    case l of {
        []      -> 0,
        x :: xs -> 1 + length xs
    }

main : Int
main = length l