-- I exercise 18

data CharList = End | List Char CharList

fromTo : Int -> Int -> CharList -> CharList
fromTo a b s = drop' a (take' (b+1) s)

drop' : Int -> CharList -> CharList
drop' i s =
    case s of {
        End -> End,
        List c rest -> if i > 0
                            then drop' (i-1) rest
                            else List c (drop' (i-1) rest)
    }

take' : Int -> CharList -> CharList
take' i s =
    case s of {
        End -> End,
        List c rest -> if i == 0
                            then End
                            else List c (take' (i-1) rest)
    }

main : CharList
main = fromTo 2 4 (List 'g' (List 'f' (List 'e' (List 'd' (List 'c' (List 'b' (List 'a' End)))))))
-- result = List 'e' (List 'd' (List 'c' End))
