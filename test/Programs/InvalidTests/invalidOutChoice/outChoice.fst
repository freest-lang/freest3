
-- It doesn't really matter, it should not throw an error here
main : Int
main = 23


fun : +{L1: !Int} -> Int
fun c =
   match c with
    L1 c1 -> let x = send 23 c1 in 23
