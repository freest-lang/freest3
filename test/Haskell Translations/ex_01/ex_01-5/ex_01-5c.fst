-- I exercise 5c

data CharList = End | List Char CharList

removeFirstAndLast : CharList -> CharList
removeFirstAndLast list = removeFirst (removeLast list)

removeFirst : CharList -> CharList
removeFirst list = 
    case list of {
        End -> End,
        List _ rest -> rest
    }

removeLast : CharList -> CharList
removeLast list = 
    case list of {
        End -> End,
        List x rest ->  case rest of {
                            End -> End,
                            List _ _ -> List x (removeLast rest) 
                        }
    }

main : CharList
main = removeFirstAndLast (List 'a' (List 'b' (List 'c' (List 'd' End))))
-- result (List 'b' (List 'c' End))