-- I exercise 10

data TripleList = Nil | List Int Int Int TripleList

listPits : Int -> TripleList
listPits x = listPits' 1 1 1 x

listPits' : Int -> Int -> Int -> Int -> TripleList
listPits' a b c max 
    | c > max = Nil
    | b > max = listPits' 1 1 (c+1) max
    | a > max = listPits' 1 (b+1) c max
    | (a*a) + (b*b) == (c*c) = List b a c (listPits' (a+1) b c max)   -- list b a instead of a b for ascending purposes
    | otherwise = listPits' (a+1) b c max

removeDups : TripleList -> TripleList
removeDups Nil               = Nil 
removeDups (List x y z rest) = List x y z (removeDups (removeDup x y rest))

removeDup : Int -> Int -> TripleList -> TripleList
removeDup x y Nil = Nil
removeDup x y (List a b c rest) 
    | (x == b) && (y == a) = removeDup x y rest
    |otherwise = List a b c (removeDup x y rest)

main : TripleList
main = removeDups (listPits 20)
-- result = List 3 4 5 (List 6 8 10 (List 5 12 13 
--         (List 9 12 15 (List 8 15 17 (List 12 16 20 Nil)))))
