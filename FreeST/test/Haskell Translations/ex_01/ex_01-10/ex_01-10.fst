-- I exercise 10

data TripleList = End | List Int Int Int TripleList

listPits : Int -> TripleList
listPits x = listPits' 1 1 1 x

listPits' : Int -> Int -> Int -> Int -> TripleList
listPits' a b c max = if (c > max)
                        then End
                        else    (if (b > max)
                                    then listPits' 1 1 (c+1) max
                                    else    (if (a > max)
                                                then listPits' 1 (b+1) c max
                                                else    (if (a*a) + (b*b) == (c*c)
                                                            then List b a c (listPits' (a+1) b c max)   -- list b a instead of a b for crescent purposes
                                                            else listPits' (a+1) b c max
                                                        )
                                            )
                                )

removeDups : TripleList -> TripleList
removeDups list = 
    case list of {
        End -> End,
        List x y z rest -> List x y z (removeDups (removeDup x y rest))
    }

removeDup : Int -> Int -> TripleList -> TripleList
removeDup x y list = 
    case list of {
        End -> End,
        List a b c rest -> if (x == b) && (y == a)
                                then removeDup x y rest
                                else List a b c (removeDup x y rest)
    }

main : TripleList
main = removeDups (listPits 100)
-- result = List 3 4 5 (List 6 8 10 (List 5 12 13 (List 9 12 15 (List 8 15 17 (List 12 16 20 
--          (List 7 24 25  (List 15 20 25 (List 10 24 26 (List 20 21 29 (List 18 24 30 
--          (List 16 30 34 (List 21 28 35 (List 12 35 37 (List 15 36 39 (List 24 32 40 
--          (List 9 40 41  (List 27 36 45 (List 14 48 50 (List 30 40 50 (List 24 45 51 
--          (List 20 48 52 (List 28 45 53 (List 33 44 55 (List 40 42 58 (List 36 48 60 
--          (List 11 60 61 (List 16 63 65 (List 25 60 65 (List 33 56 65 (List 39 52 65 
--          (List 32 60 68 (List 42 56 70 (List 48 55 73 (List 24 70 74 (List 21 72 75 
--          (List 45 60 75 (List 30 72 78 (List 48 64 80 (List 18 80 82 (List 13 84 85 
--          (List 36 77 85 (List 40 75 85 (List 51 68 85 (List 60 63 87 (List 39 80 89 
--          (List 54 72 90 (List 35 84 91 (List 57 76 95 (List 65 72 97 (List 28 96 100 
--          (List 60 80 100 End)))))))))))))))))))))))))))))))))))))))))))))))))))
