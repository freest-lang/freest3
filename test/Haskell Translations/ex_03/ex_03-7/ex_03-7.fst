-- I exercise 7

data CharList = End | List Char IntList

-- requires at least 
-- function ord (ord : Char -> Int)
-- function chr (chr : Int -> Char)

leetSpeak : CharList -> CharList
leetSpeak list =
    case list of {
        End -> End,
        List c rest -> List (translate c) leetSpeak rest
    }

translate : Char -> Char
translate c =   if ((ord c) == 65) || ((ord c) == 65+32) then '4' else -- 'A' || 'a'
                if ((ord c) == 69) || ((ord c) == 69+32) then '3' else -- 'E' || 'e'
                if ((ord c) == 73) || ((ord c) == 73+32) then '1' else -- 'I' || 'i'
                if ((ord c) == 79) || ((ord c) == 79+32) then '0' else -- 'O' || 'o'
                if ((ord c) == 83) || ((ord c) == 83+32) then '5' else -- 'S' || 's'
                if ((ord c) == 84) || ((ord c) == 84+32) then '7' else -- 'T' || 't'
                if ((ord c) > 96)  then chr (ord c - 32) else c

main : CharList
main = leetSpeak List 'A' (List 'E' (List 'I' (List 'o' (List 'S' (List 't' (List 'Z' (List 'z' Emd)))))))
-- expected result = List '4' (List '3' (List '1' (List '0' (List 'S' (List '7' (List 'Z' (List 'Z' End)))))))
