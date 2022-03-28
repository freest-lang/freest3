-- V exercise 23

gz : [[Int]] -> [[Bool]]
gz list =
    case list of {
        [] -> [],
        l :: rest -> (gz' l) :: (gz rest)
    }

gz' : [Int] -> [Bool]
gz' list =
    case list of {
        [] -> [],
        x :: rest -> (x > 0) :: (gz' rest)
    }

list1 : [Int]
list1 = [1,2,3]
list2 : [Int]
list2 = [2,-1,3,7]

list : [[Int]]
list = [list1,list2]

main : [[Bool]]
main = gz list
--result = [[True,True,True],[True,False,True,True]]