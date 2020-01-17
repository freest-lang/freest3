-- I exercise 6a

data CharList = End | List Char CharList

isPrefix : CharList -> CharList -> Bool
isPrefix prefix string = 
    case prefix of {
        End -> True,
        List c1 prefix1 ->  case string of {
                                End -> False,
                                List c2 string2 -> if (c1 == c2) -- expects Int's instead of Char's
                                                    then isPrefix prefix1 string2
                                                    else False
                            }
    }

main : Bool
main = isPrefix (List 'a' (List 'b' (List 'c' End))) (List 'a' (List 'b' (List 'c' (List 'd' End))))
-- expected result = True