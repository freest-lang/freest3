-- V exercise 12

concatLists : [Int] -> [Int] -> [Int]
concatLists list1 list2 =
    case list1 of {
        [] -> list2,
        x :: rest -> x :: (concatLists rest list2)
    }

list : [Int]
list = [1,2,3]

list12 : [Int]
list12 = [1,2]

main : [Int]
main = concatLists (concatLists (concatLists list12 list) list12) list
-- result = [1,2,1,2,3,1,2,1,2,3]