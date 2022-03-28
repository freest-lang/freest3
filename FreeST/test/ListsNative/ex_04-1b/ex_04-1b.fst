-- IV exercise 1b

replicate' : Int -> Char -> [Char]
replicate' r c = 
    if r == 0 
        then []
        else c :: (replicate' (r-1) c)

main : [Char]
main = replicate' 5 'c'
-- result = ['c','c','c','c','c']