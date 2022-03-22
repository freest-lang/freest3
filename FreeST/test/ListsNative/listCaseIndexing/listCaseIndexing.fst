
l : [Int]
l = [1,2,3,4,5]

index : [Int] -> Int
index l i =
    case l of {
        x :: xs -> 
            if i == 0 then
                x
            else 
                index xs (i-1)
    }

main : Int
main = index l 3