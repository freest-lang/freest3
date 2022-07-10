-- I exercise 8

data ListInt = End | List Int ListInt

-- without pattern matching
safeTail : ListInt -> ListInt
safeTail list = 
    case list of {
        End -> End,
        List _ rest -> rest
    }

-- with pattern matching
-- safeTailPM -> [Int] -> [Int]
-- safeTailPM [] = []
-- safeTailPM (_:xs) = xs

main : ListInt
main = safeTail End
-- main = safeTail []
-- result = End
