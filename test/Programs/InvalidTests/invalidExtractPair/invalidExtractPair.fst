
-- It doesn't really matter, it should not throw an error here
main : Int
main = 10

-- invalid extract pair: Expecting a pair type; found Bool
fun : Bool
fun =
  let x, y = extractPair in
  x

extractPair : Bool
extractPair = True
