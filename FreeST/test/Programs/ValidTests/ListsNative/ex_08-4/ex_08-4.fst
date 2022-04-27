-- VIII exercise 4

somar : [Int] -> Int
somar list =
    case list of {
        [] -> 0,
        x :: rest -> x + somar rest
    }

list : [Int]
list = [1,2,3,-10]

main : Int
main = somar list
--result = -4
