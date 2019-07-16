-- I exercise 7

data IntList = End | List Int IntList

partition : Int -> Int -> Int -> IntList
partition begin end d = makePartition begin end (div (end - begin) d)

makePartition : Int -> Int -> Int -> IntList
makePartition begin end interval =  if (begin + interval) > end 
                                        then End 
                                        else List (begin + interval) (makePartition (begin+interval) end interval)

main : IntList
main = partition 10 20 5
-- result = List 12 (List 14 (List 16 (List 18 (List 20 End))))