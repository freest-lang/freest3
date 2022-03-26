-- I exercise 7

partition : Int -> Int -> Int -> [Int]
partition begin end d = makePartition begin end (div (end - begin) d)

makePartition : Int -> Int -> Int -> [Int]
makePartition begin end interval =  
    if (begin + interval) > end 
        then [] 
        else (begin + interval) :: (makePartition (begin+interval) end interval)

main : [Int]
main = partition 10 20 5
-- result = 12 :: 14 :: 16 :: 18 :: 20 :: []