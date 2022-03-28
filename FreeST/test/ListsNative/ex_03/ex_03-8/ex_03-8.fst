-- I exercise 8


-- without pattern matching
safeTail : [Int] -> [Int]
safeTail list = 
    case list of {
        []        -> [],
        _ :: rest -> rest
    }

-- with pattern matching
-- safeTailPM -> [Int] -> [Int]
-- safeTailPM [] = []
-- safeTailPM (_:xs) = xs

main : [Int]
main = safeTail []
-- main = safeTail []
-- result = []