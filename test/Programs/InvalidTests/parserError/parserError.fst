
-- It doesn't really matter, it should not throw an error here
main : Int
main = 10

-- invalid let just to throw a parser error
fun : Bool
fun = let x, y = extractPair in

  
