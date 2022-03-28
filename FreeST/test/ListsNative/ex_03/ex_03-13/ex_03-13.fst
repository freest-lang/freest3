-- I exercise 13

-- zip xs ys
--  where xs = tail [0,1,2,3,4]
--        ys = init ['a','b','c','d']

data [Char] = CEnd | CList Char [Char]

tailInt : [Int] -> [Int]
tailInt list = 
    case list of {
        [] -> [],
        _ :: rest -> rest
    }

initChar : [Char] -> [Char]
initChar list =
    case list of {
        [] -> [],
        x :: rest -> 
            case rest of {
                [] -> [],
                _ :: _ -> x :: (initChar rest)                
            }
    }

zipList : [Int] -> [Char] -> PairList
zipList list1 list2 = 
    case list1 of {
        [] -> [],
        x :: rest1 -> 
            case list2 of {
                [] -> [],
                c :: rest2 -> (x,c) :: (zipList rest1 rest2)
            }
    }

main : [(Int,Char)]
main = zipList (tailInt  ([ 0 , 1 , 2 , 3]))
               (initChar (['a','b','c'   ]))
-- result = (1,'a') :: (2,'b') :: []