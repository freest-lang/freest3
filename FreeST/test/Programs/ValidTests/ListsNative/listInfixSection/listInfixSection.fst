

append1 : [Int] -> [Int]
append1 = (:: 1)

append2 : [Int] -> [Int]
append2 = (2 ::)

append : Int -> [Int] -> [Int]
append = (::)


main : [Int]
main = append1 $ append2 $ append 3 $ []