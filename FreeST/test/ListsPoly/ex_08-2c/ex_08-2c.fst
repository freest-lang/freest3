-- VIII exercise 2b

-- adapted for number so it can be compiled

palindrome : [Int] -> Bool
palindrome list = equals list (reverseList list)

reverseList : [Int] -> [Int]
reverseList list = reverseList' list [] 

reverseList' : [Int] -> [Int] -> [Int]
reverseList' s1 s2 = 
    case s1 of {
        []  -> s2,
        n :: rest -> reverseList' rest (n :: s2)
    }

equals : [Int] -> [Int] -> Bool
equals s1 s2 =
    case s1 of {
        []  -> 
            case s2 of {
                []  -> True,
                _ :: _ -> False
            },
        n1 :: r1 -> 
            case s2 of {
                []  -> False,
                n2 :: r2 -> 
                    if n1 /= n2         -- Char's cannot be tested equal
                        then False
                        else equals r1 r2
            }
    }

number1 : [Int]
number1 = [1,2,3,4,5,6,7,8,9,10]

number2 : [Int]
number2 = [1,2,3,4,3,2,1]

map' : ([Int] -> Bool) -> [[Int]] -> [Bool]
map' f list =
    case list of {
        [] -> [],
        s :: rest -> (f s) :: (map' f rest) 
    }

main : [Bool]
main = map' palindrome ([number1,number2])
--result = [False,True]