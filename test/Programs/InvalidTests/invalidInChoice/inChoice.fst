
-- It doesn't really matter, it should not throw an error here
main : Int
main = 23


fun : &{L1: !Int} -> Int
fun c = select L1 c
