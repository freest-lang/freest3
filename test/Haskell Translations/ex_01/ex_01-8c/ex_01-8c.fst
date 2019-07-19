-- I exercise 8c

data CharList = End | List Char CharList

isDigit : Char -> Bool
isDigit c = 
    let digits = List '0' (List '1' (List '2' (List '3' (List '4' (List '5' (List '6' (List '7' (List '8' (List '9'))))))))) in
    exists c digits

exists : Char -> CharList -> Bool
exists c1 list = 
    case list of {
        End -> False,
        List c2 rest -> if c1 == c2 
                            then True
                            else exists c1 rest
    }

getDigits : CharList -> CharList
getDigits list = 
    case list of {
        End -> End,
        List c rest ->  if isDigit c 
                            then List c (getDigits rest)
                            else getDigits rest
    }

main : CharList
main = getDigits (List 'a' (List 'b' (List '0' (List '1' End))))
-- expected result = List '0' (List '1' End)