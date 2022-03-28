-- IV exercise 1g

multiplos : [Int] -> Int -> [Int]
multiplos list x = 
    case list of {
        [] -> [],
        y :: rest -> 
            if mod y x == 0
                then y :: (multiplos rest x)
                else multiplos rest x
    }

main : [Int]
main = multiplos ([1,3,6,2,5,15,3,5]) 3
-- result = [3,6,15,3]