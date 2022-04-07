
f : (Int -> [Int] -> [Int]) -> Int -> [Int] -> [Int]
f a b c = a b c

main : [Int]
main = f (::) 1 ([2])