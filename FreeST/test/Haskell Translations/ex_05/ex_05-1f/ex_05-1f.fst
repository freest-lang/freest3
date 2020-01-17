-- V exercise 1f

data CharList = End | List Char CharList

addBreakLine : CharList -> CharList
addBreakLine list = 
    case list of {
        End -> List ('\n') End,
        List c rest -> List c (addBreakLine rest)
    }

string : CharList
string = List 'a' (List 'b' (List 'c' (List 'd' (List 'e' End))))

main : CharList
main = addBreakLine string
-- expected result = List 'a' (List 'b' (List 'c' (List 'd' (List 'e' (List '\n' End)))))