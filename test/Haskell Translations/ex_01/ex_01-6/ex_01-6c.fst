-- I exercise 6b

data CharList = End | List Char CharList

isPrefixOrSuffix : CharList -> CharList -> Bool
isPrefixOrSuffix s_or_p string = (isPrefix s_or_p string) || (isSuffix s_or_p string)

isSuffix : CharList -> CharList -> Bool
isSuffix suffix string = isPrefix (listReverse suffix) (listReverse string)

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

listReverse : CharList -> CharList
listReverse list = listShifter list End

listShifter : CharList -> CharList -> CharList
listShifter list invertedList = 
    case list of {
        End -> invertedList,
        List x rest -> listShifter rest (List x invertedList)
    }

main : Bool
main = isPrefixOrSuffix (List 'b' (End)) (List 'a' (List 'b' (List 'd' End)))
-- expected result = False