-- VIII exercise 4

data List = E | L Int List

somar : List -> Int
somar list =
    case list of {
        E -> 0,
        L x rest -> x + somar rest
    }

list : List
list = L 1 (L 2 (L 3 (L (-10) E)))

main : Int
main = somar list
--result = -4
