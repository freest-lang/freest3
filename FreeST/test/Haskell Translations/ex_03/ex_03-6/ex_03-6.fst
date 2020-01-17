-- I exercise 6

data Ordinal = Ordinal Int CharList
data CharList = End | List Char CharList

ordinalPrefix : Int -> Ordinal
ordinalPrefix x = 
    let st = List 's' (List 't' End) in
    let nd = List 'n' (List 'd' End) in
    let rd = List 'r' (List 'd' End) in
    let th = List 't' (List 'h' End) in

    if (mod x 10 == 1) && (not (mod x 100 == 11)) then Ordinal x st else
    if (mod x 10 == 2) && (not (mod x 100 == 12)) then Ordinal x nd else
    if (mod x 10 == 3) && (not (mod x 100 == 13)) then Ordinal x rd else
        Ordinal x th

main : Ordinal
main = ordinalPrefix 5111
-- result = Ordinal 5111 (List 't' (List 'h' End))