-- V exercise 13

data CharList = End | List Char CharList

isNonBlanck : Char -> Bool
isNonBlanck c = not (existsIn c (List (' ') (List '\t' (List '\n' End))))
                                            -- '\t' and '\n' does not exist

existsIn : Char -> CharList -> Bool
existsIn c list = 
    case list of {
        End -> False,
        List x rest -> 
            if c == x                       -- Can't equal two different characters
                then True
                else existsIn c rest
    }

main : Bool
main = isNonBlanck (' ')
-- expected result = False
